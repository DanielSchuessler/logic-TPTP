for x in TestImportExportImportFile TestImportExportRandom PrettyPrintFile ParseRandom; do
    
    ghc -outputdir obj -o bin/$x --make -O2 -main-is $x $x
    
done
