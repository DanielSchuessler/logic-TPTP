#!/bin/bash

set -e

mkdir -p bin
mkdir -p obj

for x in TestImportExportImportFile TestImportExportRandom PrettyPrintFile ParseRandom; do
    
    ghc -prof -auto-all -outputdir obj -o bin/$x --make -O2 -main-is $x $x
    
done
