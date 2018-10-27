build:
	stack build fcm-client
	find cli test src -type f -name '*.hs' | xargs hlint

test:
	stack test fcm-client

clean:
	stack clean

ghci:
	stack ghci

hoogle:
	stack hoogle

.PHONY: \
	build \
	clean \
	ghci \
	test
