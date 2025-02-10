#!/bin/ksh

# Credits
# Author:   Thibault Colcomb 
# Company:  EPITA - LRE
# Date:     2025-18-01
# Modified: 2025-07-02
# Description
# This script runs the testsuite for the pattern matching project.

# Print usage function
PrintUsage()
{
    echo "Usage: ./launch_testsuite.sh"
    echo "Run this script in the root directory of the testsuite."
    echo "It will run the testsuite and print the results."
    echo ""
}

# Main function RunTestSuite
RunTestSuite()
{
    # Variables
    tests=0
    successes=0
    skipped=0

    echo ""
    echo "---------- PATTERN MATCHING TESTSUITE ----------"

    # Loop over subdirectories
    for subdir in $(ls -d */)
    do
        subdir=${subdir%?}

        # cd into subdir
        # If cd fails, skip the test
        cd $subdir
        if [ $? -ne 0 ]
        then
            echo -e "\033[33m[SK] $subdir\033[0m"
            skipped=$((skipped+1))
            continue
        fi

        # Run make and check output
        gmake clean > /dev/null
        gmake -r > /dev/null
        # if result of make test displays [OK] in stdout
        if [ "$(gmake test)" = "[OK]" ]
        then
            echo -e "\033[32m[OK] $subdir\033[0m"
            successes=$((successes+1))
        else
            echo -e "\033[31m[KO] $subdir\033[0m"
        fi

        # Clean up
        make clean > /dev/null

        # cd out of subdir
        cd ..

        tests=$((tests+1))
    done

    # Echo results
    echo ""
    echo "---------- RESULTS ----------"
    echo "Tests: $tests"
    echo -e "\033[32mSuccesses: $successes\033[0m"
    echo -e "\033[31mFailures: $((tests-successes))\033[0m"
    echo -e "\033[33mSkipped: $skipped\033[0m"
    echo ""
}

main(){
    if [ $# -ne 0 ]
    then
        PrintUsage
        exit 1
    fi
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]
    then
        PrintUsage
        exit 0
    fi
    RunTestSuite
}

main "$@"
