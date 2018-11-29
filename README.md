# scrapegis

A tool for querying the Hennepin county property GIS server for data, and
dumping to CSV.

Wrote this more or less to learn more Haskell, apologies to anyone who actually needs to use it. 

## Installation

Should work with stack:

    stack build

## Usage

    scrapegis fetch city --csv [--out=<path>]
    scrapegis fetch zip <zip_code> --csv [--out=<path>]

Or for development purposes:

    stack exec -- scrapegis ... etc.

Either specify an outfile, or it will be stdout.

See Usage.txt for more advanced examples.

## How to run tests

    stack test
