module TypeChecker where

import           Data.Map                       ( Map
                                                , insert
                                                , lookup
                                                , size
                                                , fromList
                                                , empty
                                                , showTree
                                                )
import           Data.List                      ( sort
                                                , find
                                                )
import           Data.Set                       ( Set
                                                , fromList
                                                )
import           AbsVarlang
import           VarlangState
import           ErrM

data FunRetType = NotFound | Found Type | Conflict | BadErr String
    deriving (Eq, Ord, Show, Read)

-- the head is current scope, each scope in current state is represented as list element
type TypeCheckerState = [Map Ident Type]
type GetTypeResponse = Err Type
type CheckStatementResponse = Err TypeCheckerState
type CheckFunStatementResponse = Err TypeCheckerState

-- constant strings for messages
type_mismatch = "Type mismatch: "
function_not_declared_correctly = "Function is not declared correctly: "
function_redefinition = "Function with this name is already defined"
binary_operation_type_not_allowed = "This type is not allowed in this binary operation"
variable_redefenition = "Variable already was defined: "
variant_empty = "Variant can not be empty: "
variant_def_not_found = "Variant does not have the case for: "
cannot_print_type = "Printing is not supported for the type: "
match_statment_not_var = "Trying to apply match statement to object which is not variant"
match_statement_identifiers_error = "There is error in match identifiers statement"
function_return_conflict = "Function definition has conflict beacause of different return types"
check_fun_type_error = "Error in func type checking"
function_cant_have_void_args = ": function can not have void arguments"
repeated_arg_names = ": function can not have arguments with repeated names"
too_many_args = ": too many arguments"
too_few_args = ": too few arguments"
list_parser_only_for_lists = "operator : can be used only with lists"
list_append_only_for_lists = "method append can be used only with lists"

-- help variables
typeCheckerStartState = [Data.Map.empty]
eqTypes = [Int, Bool, Char]

-- FUNCTIONS

checkType :: Program -> CheckStatementResponse
checkType (ProgramL statements) = checkStatements statements typeCheckerStartState

checkStatements :: [Stm] -> TypeCheckerState -> CheckStatementResponse
checkStatements [] state = Ok state
checkStatements (stm : stmts) state =
    checkStatement stm state >>= \state -> checkStatements stmts state

checkStatement :: Stm -> TypeCheckerState -> CheckStatementResponse
checkStatement stm state = case stm of
    StmDecl (DeclL t identifiers) -> checkStmDecl t identifiers state
    StmBlock statements ->
        checkStatements statements (Data.Map.empty : state) >>= \state -> return (tail state)
    StmAss name exp -> getTypeForVariable name state >>= \varType -> getType exp state
        >>= \expType -> compareComplexTypes expType varType >>= \_ -> return state
    StmParseList headIdent tailIdent exp ->
        getType exp state >>= \expType -> case expType of
            List t -> do
                newState <- checkStmDecl t [headIdent] state
                checkStmDecl (List t) [tailIdent] newState
            _ -> Bad $ type_mismatch ++ list_parser_only_for_lists
    StmAppend ident _ ->
        getTypeForVariable ident state >>= \varType -> case varType of
            List _ -> return state
            _ -> Bad $ type_mismatch ++ list_append_only_for_lists
    StmDictAss identifier exp1 exp2 ->
        getTypeForVariable identifier state >>= \dictType -> case dictType of
            Dict keyType valType -> getType exp1 state >>= \t1 ->
                getType exp2 state
                    >>= \t2 -> compareTypes t1 keyType
                            >>= \_ -> compareTypes t2 valType >>= \_ -> return state
            _ -> Bad $ type_mismatch ++ "Dictionary " ++ show identifier
    StmIf exp stm               -> checkBoolExpWithStm exp stm state
    StmIfElse exp stmIf stmElse -> getType exp state >>= \expType ->
        compareTypes expType Bool
            >>= \_ -> checkStatement stmIf state
                    >>= \_ -> checkStatement stmElse state >>= \_ -> return state
    StmWhile exp stm -> checkBoolExpWithStm exp stm state
    StmFor forDeclaration expList stmts -> checkForStatement forDeclaration expList stmts state
    StmFunDef (Ident funName) args stmts -> case Data.Map.lookup (Ident funName) (head state) of
        Just _  -> Bad ( function_redefinition ++ funName)
        Nothing -> case (voidArgs, hasDuplicatedArgs, stateWithFunction) of
            ([], False, Ok newStateWithFunction) ->
                checkStatements stmts (Data.Map.fromList listForMap : newStateWithFunction)
                    >>= \(_ : res) -> return res
            ([], False, Bad s) -> Bad s
            (_, True, _) -> Bad $ function_not_declared_correctly ++ show funName ++ repeated_arg_names
            (_, _, _) -> Bad $ function_not_declared_correctly ++ show funName ++ function_cant_have_void_args
          where
            argNames          = map (\(ArgL _ n) -> n) args
            hasDuplicatedArgs = length argNames /= length (Data.Set.fromList argNames)
            argTypes          = map (\(ArgL t _) -> t) args
            voidArgs          = filter (== Void) argTypes
            stateWithFunction = case funRetType of
                Found t  -> return $ addFunctionToTypeCheckerState funName t argTypes state
                NotFound -> return $ addFunctionToTypeCheckerState funName Void argTypes state
                _        -> Bad function_return_conflict
            listForMap = map (\(ArgL t ident) -> (ident, t)) args
            newState   = addFunctionToTypeCheckerState funName
                                                       Void
                                                       argTypes
                                                       (Data.Map.fromList listForMap : state)
            funRetType = getFunRetType stmts newState
    StmMatch exp caseStmts -> checkMatchStatement exp caseStmts state
    StmPrint   exp         -> getType exp state >> return state
    StmStepExp exp         -> getType exp state >>= \t -> return state
    StmPrintS  _           -> return state
    RetStm _   -> Ok state
    RetVoidStm -> Ok state

checkStmDecl :: Type -> [Ident] -> TypeCheckerState -> CheckStatementResponse
checkStmDecl t identifiers state = return (newScope : tail state)
  where
    newScope = foldl insertVar (head state) identifiers
    insertVar scope name = Data.Map.insert name t scope

addFunctionToTypeCheckerState :: String -> Type -> [Type] -> TypeCheckerState -> TypeCheckerState
addFunctionToTypeCheckerState funName funRetType argTypes state =
    Data.Map.insert (Ident funName) (Fun funRetType argTypes) (head state) : tail state

getFunRetType :: [Stm] -> TypeCheckerState -> FunRetType
getFunRetType []              _     = NotFound
getFunRetType (funStm : tail) state = chooseBetterType t1 t2
  where
    t1 = getFunRetTypeForStatement funStm state
    t2 = getFunRetType tail state


chooseBetterType :: FunRetType -> FunRetType -> FunRetType
chooseBetterType t1 t2 = case (t1, t2) of
    (Conflict, _       ) -> Conflict
    (_       , Conflict) -> Conflict
    (BadErr s, _       ) -> BadErr s
    (_       , BadErr s) -> BadErr s
    (NotFound, t       ) -> t
    (t       , NotFound) -> t
    (Found t1, Found t2) -> if t1 == t2 then Found t1 else Conflict

getFunRetTypeForStatement :: Stm -> TypeCheckerState -> FunRetType
getFunRetTypeForStatement stm state = case stm of
    RetStm (EFunCall _ _) -> NotFound
    RetStm exp -> case getType exp state of
        Ok  t -> Found t
        Bad s -> BadErr $ s ++ show exp
    RetVoidStm            -> Found Void
    StmBlock stmts        -> getFunRetType stmts state
    StmIf _ stm           -> getFunRetTypeForStatement stm state
    StmIfElse _ stm1 stm2 -> getFunRetType [stm1, stm2] state
    StmWhile _ stm        -> getFunRetTypeForStatement stm state
    StmFor _ _ stmts      -> getFunRetType stmts state
    StmMatch _ caseStmts ->
        getFunRetType (foldl (\acc (CaseStmL _ _ stmts) -> stmts ++ acc) [] caseStmts) state
    _ -> NotFound

checkBoolExpWithStm :: Exp -> Stm -> TypeCheckerState -> CheckStatementResponse
checkBoolExpWithStm exp stmt state = getType exp state >>= \expType ->
    compareTypes expType Bool >>= \_ -> checkStatement stmt state >>= \state -> return state

checkMatchStatement :: Exp -> [CaseStm] -> TypeCheckerState -> CheckStatementResponse
checkMatchStatement exp caseStmts state = getType exp state >>= \t -> case t of
    Var varDeclarations -> checkCaseStatementsAndVarDeclarations caseStmts varDeclarations state
    _                   -> Bad $ type_mismatch ++ match_statment_not_var

checkCaseStatementsAndVarDeclarations
    :: [CaseStm] -> [VarD] -> TypeCheckerState -> CheckStatementResponse
checkCaseStatementsAndVarDeclarations caseStmts declarations state = if equalIdentifiers
    then checkCaseStatementsAndVarDeclarations (Data.List.sort caseStmts)
                                               (Data.List.sort declarations)
                                               state
    else Bad match_statement_identifiers_error
  where
    identifiersFromStmts        = map (\(CaseStmL ident _ _) -> ident) caseStmts
    identifiersFromDeclarations = map (\(VarDL ident t) -> ident) declarations
    equalIdentifiers =
        Data.List.sort identifiersFromStmts == Data.List.sort identifiersFromDeclarations
    checkCaseStatementsAndVarDeclarations [] [] state = return state
    checkCaseStatementsAndVarDeclarations (CaseStmL ident variable stmts : stmTail) (VarDL _ t : declTail) state
        = checkStatement (StmDecl $ DeclL t [variable]) (Data.Map.empty : state) >>= \state ->
            checkStatements stmts state >>= \state ->
                checkCaseStatementsAndVarDeclarations stmTail declTail (tail state)

checkForStatement :: Decl -> Exp -> [Stm] -> TypeCheckerState -> CheckStatementResponse
checkForStatement (DeclL tDecl ident) expList stmts state =
    checkStatement (StmDecl (DeclL tDecl ident)) (Data.Map.empty : state) >>= \state ->
        getType expList state >>= \t -> compareTypes (List tDecl) t
            >>= \_ -> checkStatements stmts state >>= \state -> return (tail state)

getType :: Exp -> TypeCheckerState -> GetTypeResponse
getType exp state = case exp of
    EIncrR identifier              -> checkUnaryOperation Int identifier state
    EIncr  identifier              -> checkUnaryOperation Int identifier state
    EDecrR identifier              -> checkUnaryOperation Int identifier state
    EDecr  identifier              -> checkUnaryOperation Int identifier state
    EIncrExp identifier exp2       -> checkComplexIntOperation identifier exp2 state
    EDecrExp identifier exp2       -> checkComplexIntOperation identifier exp2 state
    EDivExp  identifier exp2       -> checkComplexIntOperation identifier exp2 state
    EMulrExp identifier exp2       -> checkComplexIntOperation identifier exp2 state
    EModrExp identifier exp2       -> checkComplexIntOperation identifier exp2 state
    EInt  _                        -> return Int
    EChar _                        -> return Char
    EValTrue                       -> return Bool
    EValFalse                      -> return Bool
    EList exps                     -> checkListType exps state >>= \t -> return $ List t
    EVar     identifier exp2       -> getType exp2 state >>= \t -> return (Var [VarDL identifier t])
    EFun     args       statements -> checkFunType args statements state
    EFunCall identifier exps       -> checkFunCallType identifier exps state
    EDict dictDeclarations         -> checkDictType dictDeclarations state
    EDictGet identifier keyExp     -> checkDictGetType identifier keyExp state
    ENeg exp2                      -> checkUnaryExpOperation Bool exp2 state
    ENot exp2                      -> checkUnaryExpOperation Bool exp2 state
    EMul exp2 exp3                 -> checkComplexBinaryOperation [Int] Int exp2 exp3 state
    EDiv exp2 exp3                 -> checkComplexBinaryOperation [Int] Int exp2 exp3 state
    EMod exp2 exp3                 -> checkComplexBinaryOperation [Int] Int exp2 exp3 state
    EAdd exp2 exp3                 -> checkComplexBinaryOperation [Int] Int exp2 exp3 state
    ESub exp2 exp3                 -> checkComplexBinaryOperation [Int] Int exp2 exp3 state
    ELTH exp2 exp3                 -> checkComplexBinaryOperation [Int] Bool exp2 exp3 state
    ELE  exp2 exp3                 -> checkComplexBinaryOperation [Int] Bool exp2 exp3 state
    EGTH exp2 exp3                 -> checkComplexBinaryOperation [Int] Bool exp2 exp3 state
    EGE  exp2 exp3                 -> checkComplexBinaryOperation [Int] Bool exp2 exp3 state
    EEQU exp2 exp3                 -> checkComplexBinaryOperation eqTypes Bool exp2 exp3 state
    ENE  exp2 exp3                 -> checkComplexBinaryOperation eqTypes Bool exp2 exp3 state
    EAnd exp2 exp3                 -> checkComplexBinaryOperation [Bool] Bool exp2 exp3 state
    EOr  exp2 exp3                 -> checkComplexBinaryOperation [Bool] Bool exp2 exp3 state
    EVariable identifier           -> getTypeForVariable identifier state
    EVarIs exp2 identifier         -> checkIsVarType exp2 identifier state

checkUnaryOperation :: Type -> Ident -> TypeCheckerState -> GetTypeResponse
checkUnaryOperation allowedType identifier state =
    getType (EVariable identifier) state >>= \t -> compareTypes t allowedType

checkUnaryExpOperation :: Type -> Exp -> TypeCheckerState -> GetTypeResponse
checkUnaryExpOperation allowedType exp state =
    getType exp state >>= \t -> compareTypes t allowedType

checkComplexIntOperation :: Ident -> Exp -> TypeCheckerState -> GetTypeResponse
checkComplexIntOperation identifier exp state = getType (EVariable identifier) state >>= \t ->
    compareTypes t Int >>= \_ -> getType exp state >>= \rightType -> compareTypes Int rightType

checkComplexBinaryOperation :: [Type] -> Type -> Exp -> Exp -> TypeCheckerState -> GetTypeResponse
checkComplexBinaryOperation allowedTypes retType leftExp rightExp state =
    getType leftExp state >>= \leftType -> getType rightExp state
        >>= \rightType -> compareTypes leftType rightType >> return retType

multiCompareComplexTypes :: [Type] -> [Type] -> Err ()
multiCompareComplexTypes []           []           = Ok ()
multiCompareComplexTypes (t1 : tail1) (t2 : tail2) = do
    _ <- compareComplexTypes t1 t2
    multiCompareComplexTypes tail1 tail2
multiCompareComplexTypes [] _ = Bad too_many_args
multiCompareComplexTypes _ [] = Bad too_few_args

compareComplexTypes :: Type -> Type -> Err Type
compareComplexTypes (Var  _ ) (Var  t ) = return (Var t) -- TODO prior LOW - check for the ident
compareComplexTypes (List t1) (List t2) = case (t1, t2) of
    (Void, _   ) -> return (List t2)
    (_   , Void) -> return (List t1)
    (_   , _   ) -> if t1 == t2 then return (List t1) else Bad type_mismatch
compareComplexTypes t1 t2 = compareTypes t1 t2

compareTypes :: Type -> Type -> Err Type
compareTypes t1 t2 =
    if t1 == t2 then Ok t1 else Bad $ type_mismatch ++ show t1 ++ " ||| " ++ show t2

getTypeForVariable :: Ident -> TypeCheckerState -> Err Type
getTypeForVariable (Ident name) [] = Bad (variable_not_found ++ name)
getTypeForVariable identifier (curScope : nextScopes) =
    case Data.Map.lookup identifier curScope of
        Just res -> Ok res
        Nothing  -> getTypeForVariable identifier nextScopes


checkDictGetType :: Ident -> Exp -> TypeCheckerState -> GetTypeResponse
checkDictGetType identifier keyExp state =
    getTypeForVariable identifier state >>= \dictType -> case dictType of
        Dict keyType valType ->
            getType keyExp state >>= \t -> compareTypes t keyType >>= \_ -> return valType
        _ -> Bad $ type_mismatch ++ show identifier ++ "is not dictionary"

checkDictType :: [EDictD] -> TypeCheckerState -> GetTypeResponse
checkDictType []                     state = return (Dict Void Void)
checkDictType [EDictDL keyExp valExp] state = getType keyExp state
    >>= \keyType -> getType valExp state >>= \valType -> return (Dict keyType valType)
checkDictType (EDictDL keyExp valExp : tail) state = getType keyExp state >>= \keyType ->
    getType valExp state >>= \valType -> checkDictType tail state >>= \(Dict keyType2 valType2) ->
        compareTypes keyType keyType2
            >>= \_ -> compareTypes valType valType2 >>= \_ -> return (Dict keyType valType)


checkFunType :: [Arg] -> [Stm] -> TypeCheckerState -> GetTypeResponse
checkFunType args stmts state = checkStatements stmts newState >>= \_ -> case (voidArgs, funRetType) of
    ([], Found t)  -> return $ Fun t (map (\(ArgL t _) -> t) args)
    ([], NotFound) -> return $ Fun Void (map (\(ArgL t _) -> t) args)
    ([], _)        -> Bad check_fun_type_error
    (_, _)         -> Bad $ check_fun_type_error
                          ++ function_cant_have_void_args
    
  where
    argTypes   = map (\(ArgL t _) -> t) args
    voidArgs   = filter (\(ArgL t _) -> t == Void) args
    listForMap = map (\(ArgL t ident) -> (ident, t)) args
    newState   = Data.Map.fromList listForMap : state
    funRetType = getFunRetType stmts newState

multiCompareComplexTypesForFunArgs :: [Type] -> [Type] -> String -> String -> Err ()
multiCompareComplexTypesForFunArgs argTypes1 argTypes2 errPrefix errPostfix =
    case multiCompareComplexTypes argTypes1 argTypes2 of
        Bad errMsg -> Bad $ errPrefix ++ errMsg ++ errPostfix
        _ -> Ok ()

printIdent :: Ident -> String
printIdent (Ident name) = show name

checkFunCallType :: Ident -> [Exp] -> TypeCheckerState -> GetTypeResponse
checkFunCallType identifier exps state = getTypeForVariable identifier state >>= \funType ->
    case funType of
        Fun returnType argTypes -> getExpTypes exps state
            >>= \argTypes2 -> multiCompareComplexTypesForFunArgs
                argTypes
                argTypes2
                "Function arguments "
                (" in function: " ++ printIdent identifier)
            >> return returnType
        _ -> Bad $ type_mismatch ++ show identifier ++ " is not a function"


getExpTypes :: [Exp] -> TypeCheckerState -> Err [Type]
getExpTypes [] state = return []
getExpTypes (exp : tail) state =
    getType exp state >>= \t -> getExpTypes tail state >>= \tailTypes -> return (t : tailTypes)

checkIsVarType :: Exp -> Ident -> TypeCheckerState -> GetTypeResponse
checkIsVarType exp identifier state = getType exp state >>= \t -> case t of
    Var varDeclarations -> if exists
        then return Bool
        else Bad (variant_def_not_found ++ show identifier)
      where
        exists = foldl (\acc (VarDL ident _) -> ident == identifier && acc) False varDeclarations
    _ -> Bad $ type_mismatch ++ "is not a variant"

checkListType :: [Exp] -> TypeCheckerState -> GetTypeResponse
checkListType []            state = return Void
checkListType [exp        ] state = getType exp state >>= \t -> return t
checkListType (hExp : exps) state = getType hExp state
    >>= \hType -> checkListType exps state >>= \tType -> compareTypes hType tType

checkForTheSameElements :: Eq a => [a] -> Bool
checkForTheSameElements []      = True
checkForTheSameElements [_    ] = True
checkForTheSameElements (h : t) = checkForTheSameElements t && (head t == h)
