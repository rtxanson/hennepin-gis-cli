# scrapegis

TODO: Write description here

## Installation

    cabal configure
    cabal build
    cabal install

This depends on at least one package not yet available in `cabal`: 

 - https://github.com/docopt/docopt.hs

## Usage

    scrapegis query --csv "QUERY_STRING = BLAH" > output.csv

See Usage.txt for more advanced examples.

## How to run tests

There are no tests yet.

```
cabal configure --enable-tests && cabal build && cabal test
```
