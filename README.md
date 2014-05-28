# scrapegis

A tool for querying the Hennepin county property GIS server for data, and
dumping to CSV.

## Installation

For testing:

    make all_sandbox
    make install

This will create a sandbox and build all dependencies and stuff there. Binary ends up in:

    ./dist/build/scrapegis/scrapegis

But if you install, it will end up in the sandbox install path:

    ./sandbox/bin/scrapegis

Otherwise for reals:

    make all
    make install

And this will install like normal to a system location (usr/local/bin or whatever)

### Cabal reference 
#### (ignore if above works)

    ( cabal sandbox init --sandbox sandbox )
    ( cabal install --only-dependencies --enable-tests )

    cabal configure
    cabal build
    cabal install

Otherwise inspect the Makefile for commands.

Cabal dependencies take a while to install because there are lots (welcome to
Haskell), but once that's done, the actual package builds fast.

## Usage

    scrapegis fetch city --csv

See Usage.txt for more advanced examples.

## How to run tests

There are no tests yet.

```
cabal configure --enable-tests && cabal build && cabal test
```
