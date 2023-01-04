test="test-prettier/toPrettify.ref"
dummy="test-prettier/dummy.ref"

cat $test > $dummy
cabal v2-run Refal --verbose=0 $dummy -- -p
cd test-prettier
dif=$(diff dummy.ref expected.ref)
if [ $? -eq 0 ] ; then
        echo -e "${GREEN}[Test all] done${NC}"
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