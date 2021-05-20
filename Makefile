.PHONY: default sync build test clean ci

default: sync build test

sync:
	./tools/sync

# Build all targets, including tests.
build: sync
	stack test --no-run-tests --pedantic

# Runs the test suite.
test: build
	stack test --pedantic

# Clean build artifacts
clean:
	stack clean

# Used in the 'script' portion of .travis.yml
ci:
	./tools/sync --validate
	stack --no-terminal --skip-ghc-check test --pedantic
