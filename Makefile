list-files = find . -name '*.hs' | grep -v '.stack-work'

stack = stack --allow-different-user

ormolu = $(stack) exec -- ormolu -o '-XImportQualifiedPost' -o '-XPatternSynonyms' -o '-XTypeApplications'

format:
	@$(ormolu) --mode inplace $(shell $(list-files))

format-check:
	@$(ormolu) --mode check $(shell $(list-files))

.PHONY: format format-check
