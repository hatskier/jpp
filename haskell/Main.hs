-- JPP Haskell
-- Pierwsze zadanie zaliczeniowe
-- Aliaksei Suvorau as374118

module Main where

import           Lib
import           System.Environment
import           Text.Read                      ( readMaybe )
import           Control.Monad
import           Control.Monad.State     hiding ( State )
import           Data.Maybe

data State = State {
  stack :: [Int],
  start :: Maybe Point,
  current :: Point,
  picture :: Picture,
  pathLength :: Int,
  transformation :: Transform
}

type StateTransform a = StateT State Maybe a

startState = State
    { stack          = []
    , start          = Nothing
    , current        = point (0, 0)
    , picture        = Pic []
    , pathLength     = 0
    , transformation = Tr []
    }

prolog = "300 400 translate"
epilog = "stroke showpage"
errorText = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
usage = "Usage: ./executable scale"

ratP :: (Int, Int) -> (R, R)
ratP (a, b) = (toRational a, toRational b)

get2Numbers :: StateTransform (Int, Int)
get2Numbers = do
    s <- get
    case stack s of
        a : b : tail -> do
            put $ s { stack = tail }
            return (b, a)
        _ -> lift Nothing

getNumber :: StateTransform (Int)
getNumber = do
    s <- get
    case stack s of
        a : tail -> do
            put $ s { stack = tail }
            return (a)
        _ -> lift Nothing

lineTo :: Point -> State -> State
lineTo next state = state { current    = next
                          , pathLength = (pathLength state) + 1
                          , picture    = (picture state) & (line (curX, curY) (nextX, nextY))
                          }
  where
    Pt curX  curY  = current state
    Pt nextX nextY = next

addTransformToState :: Transform -> State -> State
addTransformToState tr state =
    state { transformation = composeTransforms tr (transformation state) }

analyzeToken :: String -> StateTransform ()

analyzeToken "moveto" = do
    (a, b)         <- get2Numbers
    start          <- gets start
    transformation <- gets transformation
    let nextPoint = trpoint transformation (point (ratP (a, b)))
    case start of
        Nothing -> modify $ \s -> s { current = nextPoint, start = Just nextPoint }
        _       -> modify $ \s -> s { current = nextPoint }

analyzeToken "lineto" = do
    (a, b) <- get2Numbers
    start  <- gets start
    guard (isJust start)
    transformation <- gets transformation
    let nextPoint = trpoint transformation (point (ratP (a, b)))
    modify $ lineTo nextPoint

analyzeToken "closepath" = do
    pathLength <- gets pathLength
    unless
        (pathLength == 0)
        (do
            Just start <- gets start
            modify $ lineTo start
        )

analyzeToken "translate" = do
    (a, b) <- get2Numbers
    modify $ addTransformToState (translate (vec (ratP (a, b))))

analyzeToken "rotate" = do
    r <- getNumber
    modify $ addTransformToState (rotate (toRational r))

analyzeToken "add" = do
    (a, b) <- get2Numbers
    modify $ \state -> state { stack = (a + b) : stack state }

analyzeToken "sub" = do
    (a, b) <- get2Numbers
    modify $ \state -> state { stack = (a - b) : stack state }

analyzeToken "div" = do
    (a, b) <- get2Numbers
    guard (b /= 0)
    modify $ \state -> state { stack = (a `div` b) : stack state }

analyzeToken "mul" = do
    (a, b) <- get2Numbers
    modify $ \state -> state { stack = (a * b) : stack state }

-- Próba odczytywania liczby
analyzeToken token = case (readMaybe token) of
    Just number -> modify $ \state -> state { stack = (number : (stack state)) }
    _           -> lift Nothing

runProgram :: State -> [String] -> Maybe State
runProgram state tokens =
    let t = foldM (\() -> analyzeToken) () tokens in fmap snd $ runStateT t state

process :: Int -> String -> [String]
process scale s = case (runProgram startState (words s)) of
    Just finishState -> showPicture (picture finishState) scale
    _                -> [errorText]

showPicture :: Picture -> Int -> [String]
showPicture pic scale = map showIntLine (renderScaled scale pic)
  where
    showIntLine ((x, y), (x', y')) =
        (show x) ++ " " ++ (show y) ++ " moveto " ++ (show x') ++ " " ++ (show y') ++ " lineto"

parseArgs :: [String] -> Maybe Int
parseArgs []  = Just 1
parseArgs [s] = case s of
    '+' : tail -> Text.Read.readMaybe tail
    _          -> Text.Read.readMaybe s
parseArgs _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case (parseArgs args) of
        Just scale -> do
            input <- getContents
            putStrLn prolog
            mapM_ putStrLn $ process scale input
            putStrLn epilog
        _ -> putStrLn usage
