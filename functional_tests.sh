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
declare -x INFILE FUNC OPAR
declare -i PARAMETER NBFAIL=0 NBTEST=1 EXIT
declare -a TESTS
echo '' > resultRuns.log

setUp(){
    INFILE="data/agriculture.tsv,data/industry.tsv,data/politic.tsv"
    EXIT=0
    PARAMETER=0
    FUNC=${FUNCNAME[1]}
    TESTS=()
    printf "\n- ${FUNC}: "
}

tearDown(){
    rm -r temp/
    mkdir temp/
}

########### ERRORS CATCH ###########

testError(){
    local BOOLEAN_ERR="false"
    local MSG=""

    [ $1 -ne ${EXIT} ] && {
        MSG=${MSG}"Program exited with bad error code: $1.\n"
        BOOLEAN_ERR="true"
    }


     [ ${EXIT} -eq 0 ] && {

        for i in ${OUTFILES[@]}; do
	    	testFileExist ${i}
	    done

	    [ ${FILE_ERR} == "true" ] && {
           MSG=${MSG}"not created.\n"
           BOOLEAN_ERR="true"
        }
     }

    tearDown

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
	printf "\n\n$NBTEST. ${TESTS[$PARAMETER]}\n" >> resultRuns.log 2>&1
    let NBTEST+=1
    let PARAMETER+=1
    Rscript  rgcca.R -d ${INFILE} ${O_PAR} $@ >> resultRuns.log 2>&1
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
        run "-d $INFILE ${TESTS[i]}"
        local ACTUAL_EXIT=$?
        printError ${ACTUAL_EXIT} ${i}
    done
}

testsDefault(){
    setUp
    TESTS=( '' )
    test
}

testsSep(){
    setUp
    TESTS=( '-s 1' )
    test
}

badTestsSep(){
    setUp
    EXIT=1
    TESTS=( '-s 4' )
    test
}

testsScheme(){
    setUp
    for i in `seq 0 3`; do
        let j=${i}+1
        TESTS[i]='-g '${j}
    done
    test
}

badTestsScheme(){
    setUp
    EXIT=1
    TESTS=( '-g 0' '-g 5' )
    test
}

testsResponse(){
    setUp
    TESTS=( '-r data/response.tsv' '-r data/response2.tsv' '-r data/response3.tsv' )
    test
}

testsConnection(){
    setUp
    TESTS=( '-c data/connection.tsv' )
    test
}

testHeader(){
    setUp
    EXIT=1
    TESTS=( '-H' )
    test
}

testExcel(){
    setUp
    INFILE="data/blocks.xlsx"
    TESTS=( '' )
    test
}

########### MAIN ###########

START_TIME=$(date -u -d $(date +"%H:%M:%S") +"%s")
#printf "Tests in progress, could take an hour...\n"
#[ -d bad ] && rm -rf bad/
mkdir temp/
setOutputPar

testsDefault
testsSep
badTestsSep
testsScheme
badTestsScheme
testsResponse
testsConnection
testHeader
testExcel

rm -r temp/
printf "\n$NBTEST tests, $NBFAIL failed.$ERRORS\n"
getElapsedTime ${START_TIME}
[[ -z ${ERRORS} ]] || exit 1
exit 0