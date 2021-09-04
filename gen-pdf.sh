#!/bin/bash

set -eu

runhaskell -isrc src/CrowdOfFunctionalProgrammers.hs > crowd-of-fp.eps
runhaskell -isrc src/Fish.hs > fish.eps
runhaskell -isrc src/Hilbert.hs > hilbert.eps
runhaskell -isrc src/RhinecanthusRectangulus.hs > school-of-humuhumu.eps
runhaskell -isrc src/Wave.hs > wave.eps

epspdf crowd-of-fp.eps
epspdf fish.eps
epspdf hilbert.eps
epspdf school-of-humuhumu.eps
epspdf wave.eps

