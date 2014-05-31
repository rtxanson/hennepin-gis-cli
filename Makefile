NUMBER_OF_CORES = $(sysctl -n hw.ncpu)
BUILD_FLAGS := "-j$(NUMBER_OF_CORES)"

# desc "Run tests"
# task :test do
#   sh "cabal install #{build_flags} --only-dependencies --enable-tests"
#   sh "cabal configure --enable-tests"
#   sh "cabal build"
#   sh "cabal test"
# end

build_sandbox: init_sandbox deps
	cabal install $(BUILD_FLAGS) --only-dependencies
	cabal configure
	cabal build

doc:
	@echo "Generate haddock documentation"
	cabal haddock
	open dist/doc/html/hi/index.html

# release: init deps
# 	set -o errexit
# 	set -o nounset
# 	cabal clean
# 	cabal sdist
# 	cd dist/
# 	tar xvf hi-*.tar.gz
# 	cd hi-*/
# 	cabal install --only-dependencies --enable-tests
# 	cabal configure --enable-tests --disable-optimization --disable-library-profiling
# 	cabal build && cabal test

deps:
	cabal install --only-dependencies --enable-tests

# cabal install --only-dependencies --enable-tests
# cabal configure --enable-tests --disable-optimization --disable-library-profiling
# cabal build && cabal test

init_sandbox:
	cabal sandbox init --sandbox sandbox

sandbox: init_sandbox

all:
	cabal configure
	cabal build

global-install:
	cabal install --global

clean_sandbox:
	cabal sandbox delete
	rm -rf sandbox/

clean: clean_sandbox
	rm -rf dist

all_sandbox: build_sandbox

