code-dirs = src test

default:
	cabal build

hlint:
	hlint --no-exit-code $(code-dirs)

stylish:
	@find $(code-dirs) -type f -name "*.hs" | while read fname; do \
	  stylish-haskell -i "$$fname"; \
	done
