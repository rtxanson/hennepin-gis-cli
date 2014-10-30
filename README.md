# scrapegis

A tool for querying the Hennepin county property GIS server for data, and
dumping to CSV.

## Installation

    make global-install

This will install like normal to a system location (/usr/local/bin)

If it complains of missing dependencies, `make deps` should be sufficient, but
don't do that unless there is a complaint.

## Usage

    scrapegis fetch city --csv [--out=<path>]
    scrapegis fetch zip <zip_code> --csv [--out=<path>]

Either specify an outfile, or it will be stdout.

See Usage.txt for more advanced examples.

## How to run tests

There are no tests yet.

```
cabal configure --enable-tests && cabal build && cabal test
```

## Developing

    make sandbox
    make deps
    make install

This will create a sandbox and build all dependencies and stuff there. Binary ends up in:

    ./dist/build/scrapegis/scrapegis

But if you install, it will end up in the sandbox install path:

    ./sandbox/bin/scrapegis

### Cabal reference 

If this isn't enough probably you need to be using cabal (haskell package
manager and build tool) for things.

#### (ignore if above works)

    ( cabal sandbox init --sandbox sandbox )
    ( cabal install --only-dependencies --enable-tests )

    cabal configure
    cabal build
    cabal install

Otherwise inspect the Makefile for commands.

Cabal dependencies take a while to install because there are lots (welcome to
Haskell), but once that's done, the actual package builds fast.
