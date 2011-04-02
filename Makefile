
all: dist/build/sag/sag

HS_FILES=src/AGParser.hs src/Main.hs src/CFG.hs src/AG.hs src/AGAbsSyn.hs src/DirectDependencies.hs

dist/build/sag/sag: SAG.cabal $(HS_FILES)
	runhaskell Setup configure
	runhaskell Setup build
