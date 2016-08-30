build:
		stack build fcm-client

test:
		stack test fcm-client

clean:
		stack clean

ghci:
		stack ghci

tags:
		hasktags-generate .

.PHONY: build test clean ghci tags
