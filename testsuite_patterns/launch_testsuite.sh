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
    echo "Usage: ./launch_testsuite.sh [-b binary]"
    echo "Run this script in the root directory of the testsuite."
    echo "It will run the testsuite and print the results."
    echo ""
    echo "Options:"
    echo "  -b binary   Specify the make binary to use (default: /bin/make)"
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
        $MAKE_BINARY clean > /dev/null
        $MAKE_BINARY > /dev/null 2>&1

        # if last 4 characters of make test output are [OK]
        if [ "$($MAKE_BINARY test | tail -c 5)" = "[OK]" ]
        then
            echo -e "\033[32m[OK] $subdir\033[0m"
            successes=$((successes+1))
        else
            echo -e "\033[31m[KO] $subdir\033[0m"
        fi

        # Clean up
        $MAKE_BINARY clean > /dev/null

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
    MAKE_BINARY="/usr/local/bin/gmake"

    while getopts ":b:h" opt; do
        case $opt in
            b)
                MAKE_BINARY=$OPTARG
                ;;
            h)
                PrintUsage
                exit 0
                ;;
            \?)
                echo "Invalid option: -$OPTARG" >&2
                PrintUsage
                exit 1
                ;;
            :)
                echo "Option -$OPTARG requires an argument." >&2
                PrintUsage
                exit 1
                ;;
        esac
    done
    shift $((OPTIND -1))

    if [ $# -ne 0 ]
    then
        PrintUsage
        exit 1
    fi

    RunTestSuite
}

main "$@"
