#!/bin/bash

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

cabal update
cabal build

cd test-integration/tests
tests=`ls *.go | sed 's/\.go$//'`
cd ../..

for test in $tests
do
    cat "test-integration/tests/$test.go" | cabal run --verbose=0 > "test-integration/tests/$test.res"
    dif=$(diff "test-integration/tests/$test.res" "test-integration/tests/$test.out")
    if [ $? -eq 0 ] ; then
        echo -e "${GREEN}[Test $test] done${NC}"
    else
        echo -e "${RED}[Test $test] failed ${NC}"
        echo -e "${YELLOW}Expected:${NC}"
        cat "test-integration/tests/$test.out"
        echo -e "${YELLOW}Actual:${NC}"
        cat "test-integration/tests/$test.res"
        exit 1
    fi
done

rm test-integration/tests/*.res

exit 0
