#!/bin/bash

set -eu

OUT_DIR=eps

rm -rf ${OUT_DIR}
mkdir -p ${OUT_DIR}

runhaskell -isrc src/CrowdOfFunctionalProgrammers.hs > ${OUT_DIR}/crowd-of-fp.eps
runhaskell -isrc src/Fish.hs > ${OUT_DIR}/fish.eps
runhaskell -isrc src/Hilbert.hs > ${OUT_DIR}/hilbert.eps
runhaskell -isrc src/RhinecanthusRectangulus.hs > ${OUT_DIR}/school-of-humuhumu.eps
runhaskell -isrc src/Wave.hs > ${OUT_DIR}/wave.eps

for f in `ls ${OUT_DIR}/*.eps`; do
    epspdf $f
done
