#!/bin/bash
# multiOmics Toolbox
# Version: 1.0
# Author: Etienne CAMENEN
# Key-words: omics, RGCCA, multi-block
# EDAM operation: analysis, correlation, visualisation
# Contact: arthur.tenenhaus@l2s.centralesupelec.fr
# Short description: performs multi-variate analysis (PCA, CCA, PLS, RGCCA) and projects the variables and samples into a bi-dimensional space.

#Settings files
FILE_ERR="false"
OUTFILES=( 'samples_space.pdf' 'variables_space.pdf' 'best_biomarkers.pdf' )

#Initialization
declare -x INFILE FUNC OPAR WARN
declare -i PARAMETER NBFAIL=0 NBTEST=0 EXIT
declare -a TESTS WARNS EXITS
echo '' > resultRuns.log

setUp(){
    INFILE="data/agriculture.tsv,data/industry.tsv,data/politic.tsv"
    EXIT=0
    PARAMETER=0
    WARN="fgHj4yh"
    WARNS=()
    FUNC=${FUNCNAME[1]}
    TESTS=()
    EXITS=()
    printf "\n- ${FUNC}: "
}

tearDown(){
#    rm -r temp/
#    mkdir temp/
    echo''
}

########### ERRORS CATCH ###########

testError(){
    local BOOLEAN_ERR="false"
    local MSG=""
    local ACTUAL_OUTPUT=$(cat temp/log | tr '\n' ' ' )

    [ $1 -ne ${EXIT} ] && {
        MSG=${MSG}"Program exited with bad error code: $1.\n"
        BOOLEAN_ERR="true"
    }

     if [ $1 -eq 0 ]
     then

        for i in ${OUTFILES[@]}; do
	    	testFileExist ${i}
	    done

	    [ ${FILE_ERR} == "true" ] && {
           MSG=${MSG}"not created.\n"
           BOOLEAN_ERR="true"
        }

     else

        if [[ ${ACTUAL_OUTPUT} != *"$WARN"* ]]; then
            MSG=${MSG}"Expected warnings not found.\n"
            echo "Expected: $WARN\n" >> warnings.log
            echo "Actual: $ACTUAL_OUTPUT"  >> warnings.log
            BOOLEAN_ERR="true"
        fi
    fi

    [ ${BOOLEAN_ERR} == "true" ] && {
	    ERRORS=${ERRORS}"\n***************\n##Test \"${TESTS[$2]}\" in $FUNC: \n$MSG"
	    return 1
    }
    return 0
}

testFileExist(){

   [ ! -f "temp/"$1 ]  && {
        MSG=${MSG}"$1 "
        FILE_ERR="true"
    }
}

printError(){
    testError $@
    if [ $? -ne 0 ]; then
        echo -n "E"
        let NBFAIL+=1
    else echo -n "."
    fi
}

########### RUN PROCESS ###########

run(){
    let NBTEST+=1
	printf "\n\n$NBTEST. ${TESTS[$PARAMETER]}\n" >> resultRuns.log 2>&1
    let PARAMETER+=1
    Rscript  rgcca.R -d ${INFILE} ${O_PAR} $@ > temp/log 2>&1
}

getElapsedTime(){
    local END_TIME=$(date -u -d $(date +"%H:%M:%S") +"%s")
    local ELAPSED_TIME=$(date -u -d "0 $END_TIME sec - $1 sec" +"%H:%M:%S")
    echo "Time to run the process ${ELAPSED_TIME:0:2}h ${ELAPSED_TIME:3:2}min ${ELAPSED_TIME:6:2}s"
}

setOutputPar(){
    OPAR=""
    for i in `seq 0 $((${#OUTFILES[@]} -1))`; do
        let j=${i}+1
        O_PAR=${O_PAR}"--output"${j}" temp/"${OUTFILES[i]}" "
    done
}

########### TESTS ###########

test(){
    for i in `seq 0 $((${#TESTS[@]} -1))`; do
        run "-d $INFILE ${TESTS[i]}" > temp/log 2>&1
        local ACTUAL_EXIT=$?
        cat temp/log >> resultRuns.log

        if [ ! -z $1 ]; then
            WARN="${WARNS[i]}"
            if [ $1 == "2" ]; then
                EXIT="${EXITS[i]}"
            fi
        fi

        printError ${ACTUAL_EXIT} ${i}

        tearDown
    done
}

testsDefault(){
    setUp
    TESTS=( '' )
    test
}

testsBlocksBad(){
    cat data/agriculture.tsv | head -n -1 > temp/agriculture2.tsv
    setUp
    INFILE="temp/agriculture2.tsv,data/industry.tsv,data/politic.tsv"
    EXIT=1
    WARN="The number of rows is different among the blocks."
    TESTS=( '' )
    test
}

testsSep(){
    setUp
    TESTS=( '-s 1')
    test
}

testsSepBad(){
    setUp
    EXIT=1
    WARN="--separator must be comprise between 1 and 2 (1: Tabulation, 2: Semicolon, 3: Comma) [by default: 2]."
    WARNS=( "${WARN}" "${WARN}" "agriculture block file has an only-column. Check the --separator [by default: 1 for tabulation]." )
    TESTS=( '-s 0' '-s 4' '-s 2' )
    test 1
}

testsScheme(){
    setUp
    for i in `seq 0 3`; do
        let j=${i}+1
        TESTS[i]='-g '${j}
    done
    test
}

testsSchemeBad(){
    setUp
    EXIT=1
    WARN="--scheme must be comprise between 1 and 4 [by default: 2]."
    TESTS=( '-g 0' '-g 5' )
    test
}

testsResponse(){
    setUp
    TESTS=( '-r data/response.tsv' '-r data/response2.tsv' '-r data/response3.tsv' )
    test
}

testsResponseBad(){
    cat data/response.tsv | head -n -1 > temp/response.tsv
    paste data/agriculture.tsv data/response.tsv > temp/response2.tsv
    setUp
    EXITS=(1 1 1 0)
    WARNS=( "The number of rows of the response file is different from those of the blocks. Possible mistake: header parameter is disabled, check if the file does'nt have one." "test.tsv file does not exist" "Please, select a response file with either qualitative data only or quantitative data only. The header must be disabled for quantitative data and activated for disjunctive table." "There is multiple columns in the response file. By default, only the first one is taken in account.")
    TESTS=( '-r temp/response.tsv' '-r test.tsv' "-r temp/response2.tsv" "-r data/agriculture.tsv")
    test 2
}

testsConnection(){
    setUp
    TESTS=( '-c data/connection.tsv' )
    test
}


testsConnection(){
    setUp
    EXIT=1
    WARNS=( "The connection file must contains only 0 or 1." "The diagonal of the connection matrix file must be 0." "The connection file must be a symmetric matrix." "The number of rows/columns of the connection matrix file must be equals to the number of files in the dataset + 1 (4)." )
    cat data/connection.tsv | tr '[1]' '[2]' > temp/connection.tsv
    cat data/connection.tsv | tr '[0]' '[2]' > temp/connection2.tsv
    cat data/connection.tsv | head -n -1 > temp/connection3.tsv
    cat data/connection.tsv | head -n -1 | cut -f -3  > temp/connection4.tsv
    TESTS=( '-c temp/connection.tsv' '-c temp/connection2.tsv' '-c temp/connection3.tsv' '-c temp/connection4.tsv' )
    test 1
}

testHeader(){
    setUp
    EXIT=1
    WARN="agriculture file contains qualitative data. Please, transform them in a disjunctive table. Possible mistake: header parameter is disabled, check if the file does'nt have one."
    TESTS=( '-H' )
    test
}

testExcel(){
    setUp
    INFILE="data/blocks.xlsx"
    TESTS=( '' )
    test
}

testFileCharacter(){
    paste data/agriculture.tsv data/response.tsv > temp/dummyFile.tsv
    setUp
    EXIT=1
    WARN="dummyFile file contains qualitative data. Please, transform them in a disjunctive table."
    INFILE="temp/dummyFile.tsv"
    TESTS=( '' )
    test
}

########### MAIN ###########

START_TIME=$(date -u -d $(date +"%H:%M:%S") +"%s")
#printf "Tests in progress, could take an hour...\n"
mkdir temp/
setOutputPar

testsResponseBad
: '
testsDefault
testsSep
testsSepBad
testsScheme
testsSchemeBad
testsResponse
testsConnection
testHeader
testExcel
testFileCharacter
testsResponseBad
testsConnection
testsBlocksBad
'

#rm -r temp/
printf "\n$NBTEST tests, $NBFAIL failed.$ERRORS\n"
getElapsedTime ${START_TIME}
[[ -z ${ERRORS} ]] || exit 1
exit 0