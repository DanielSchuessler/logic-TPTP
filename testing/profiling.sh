#!/bin/bash

set -e

mkdir -p bin
mkdir -p obj

x=Prof
ghc -fforce-recomp -prof -auto-all -outputdir obj -o bin/$x --make -O2 -main-is $x $x
    
echo "Running..."
bin/$x +RTS -P -RTS 1000
x-www-browser "$x.prof"

