all: Parser.hs Main.hs
	ghc Main.hs

Parser.hs: Parser.y
	happy Parser.y

old: Parser.hs Old.hs
	ghc Old.hs

clean:
	-rm *.hi
	-rm *.o
	-rm Parser.hs
	-rm Parser.info

clean-all: clean
	-rm Main
	-rm Old
