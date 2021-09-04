#!/bin/bash

set -eu

rm *.eps *.pdf

runhaskell -isrc src/CrowdOfFunctionalProgrammers.hs > crowd-of-fp.eps
runhaskell -isrc src/Fish.hs > fish.eps
runhaskell -isrc src/Hilbert.hs > hilbert.eps
runhaskell -isrc src/RhinecanthusRectangulus.hs > school-of-humuhumu.eps
runhaskell -isrc src/Wave.hs > wave.eps

for f in `ls *.eps`; do
    epspdf $f
done
