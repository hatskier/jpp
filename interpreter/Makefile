all: test.vl
	ghc --make Interpreter.hs -o interpreter
grammar: varlang.cf
	bnfc -haskell varlang.cf
	happy -gca ParVarlang.y
	alex -g LexVarlang.x
	bnfc -haskell varlang.cf
clean:
	rm *.o
	rm *.hi
test: test.sh
	bash test.sh
