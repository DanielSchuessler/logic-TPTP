#!/bin/bash

set -e

mkdir -p bin
mkdir -p obj

for x in Prof; do
    
    ghc -fforce-recomp -prof -caf-all -outputdir obj -o bin/$x --make -O2 -main-is $x $x
    
done
