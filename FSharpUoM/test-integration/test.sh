#!/bin/bash

# Copyright 2022-2023 Azim Muradov
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

RED='\033[1;31m'
YELLOW='\033[0;33m'
GREEN='\033[1;32m'
NC='\033[0m'

cabal build

cd test-integration/tests
tests=`ls *.fs | sed 's/\.fs$//'`
cd ../..

for test in $tests; do
    cabal run FSharpUoMExec --verbose=0 -- -f test-integration/tests/${test}.fs > test-integration/tests/${test}.res
    
    diff test-integration/tests/${test}.res test-integration/tests/${test}.out &>/dev/null
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}[Test \"${test}\"] - OK${NC}"
    else
        cat -An test-integration/tests/${test}.out >test-integration/tests/${test}.out.show
        cat -An test-integration/tests/${test}.res >test-integration/tests/${test}.res.show
        
        echo -e "${RED}[Test \"${test}\"] - FAIL${NC}\n"
        echo -e "${YELLOW}Expected:${NC}\n${YELLOW}Actual:${NC}\n" | xargs -d '\n' printf '%-47s  %-47s\n'
        diff -y -W 80 test-integration/tests/${test}.out.show test-integration/tests/${test}.res.show
        
        rm test-integration/tests/*.res
        rm test-integration/tests/*.show
        
        exit 1
    fi
done

rm test-integration/tests/*.res

exit 0