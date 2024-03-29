.PHONY: help
help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: ghcid-devel
ghcid-devel: ## Run the server in fast development mode. See DevelMain for details.
	ghcid \
	    --command "stack ghci htmx-hotreload:lib" \
	    --test "DevelMain.update"

