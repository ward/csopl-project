all: Parser.hs Main.hs
	ghc Main.hs

Parser.hs: Parser.y
	happy Parser.y


clean:
	-rm *.hi
	-rm *.o
	-rm Parser.hs

clean-all: clean
	-rm Main