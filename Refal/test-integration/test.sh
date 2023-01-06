tests=$(find test-integration/ -type f -name "*.ref")

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

for test in $tests
do
    echo $test >>  test-integration/actual
    cabal v2-run Refal --verbose=0 $test -- -i >>  test-integration/actual
done
cd test-integration
dif=$(diff expected actual)
if [ $? -eq 0 ] ; then
        echo -e "${GREEN}[Refal programs test] done${NC}"
    else
        echo -e "${RED}[Test $test] failed ${NC}"
        echo -e "${YELLOW}Expected:${NC}"
        cat expected
        echo -e "${YELLOW}Actual:${NC}"
        cat actual
        exit 1
    fi
rm actual

cd ..

prerry_test="test-prettier/toPrettify.ref"
dummy="test-prettier/dummy.ref"

cat $prerry_test > $dummy
cabal v2-run Refal --verbose=0 $dummy -- -p
cd test-prettier
dif=$(diff dummy.ref expected.ref)
if [ $? -eq 0 ] ; then
        echo -e "${GREEN}[Pretty test] done${NC}"
    else
        echo -e "${RED}[Test $test] failed ${NC}"
        echo -e "${YELLOW}Expected:${NC}"
        cat expected.ref
        echo -e "${YELLOW}Actual:${NC}"
        cat dummy.ref
        exit 1
    fi
rm dummy.ref
exit 0