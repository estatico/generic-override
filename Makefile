.PHONY: default yaml clean build test

default: yaml build test

# Ensures the package.yaml headers are up-to-date.
yaml:
	./tools/gen-package-yaml-header

# Build all targets, including tests.
build: yaml
	stack test --no-run-tests --pedantic

# Runs the test suite.
test: build
	stack test --pedantic

# Clean build artifacts
clean:
	stack clean
