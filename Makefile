format:
	fourmolu -i `find src -name "*.hs"`
tools:
	cabal install fourmolu-0.13.0.0