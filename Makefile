.PHONY: default license yaml clean build test

default: license yaml build test

# Synchronizes the LICENSE file to all subprojects
license:
	cp ./LICENSE ./aeson/

# Ensures the package.yaml headers are up-to-date.
yaml:
	./tools/gen-package-yaml-header

# Build all targets, including tests.
build: license yaml
	stack test --no-run-tests --pedantic

# Runs the test suite.
test: build
	stack test --pedantic

# Clean build artifacts
clean:
	stack clean
