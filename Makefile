list-files = find . -name '*.hs' | grep -v '.stack-work'

ormolu = stack exec -- ormolu

format:
	@$(ormolu) --mode inplace $(shell $(list-files))

format-check:
	@$(ormolu) --mode check $(shell $(list-files))

.PHONY: format format-check
