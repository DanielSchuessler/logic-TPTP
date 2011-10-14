#!/bin/bash

set -e
source compileTests.sh
test -d Problems || {
    echo "Please create a symlink 'Problems' to the 'Problems' subdirectory of the TPTP distribution (http://www.cs.miami.edu/~tptp/)"
    exit 99
}
echo "Running (import . export . import) test"
find -H Problems -type f | bin/TestImportExportImportFile False
echo "Running (import . export) test on random data"
bin/TestImportExportRandom
