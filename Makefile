SOURCEDIR := src
SOURCES := $(shell find $(SOURCEDIR) -name '*.hs')

dist/main: dist src/Main.hs $(SOURCES)
	ghc -W -isrc src/Main.hs -o $@ -hidir dist -odir dist

dist:
	mkdir dist

