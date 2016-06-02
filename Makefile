
dist/main: dist src/Main.hs
	ghc -isrc src/Main.hs -o $@ -hidir dist -odir dist

dist:
	mkdir dist

