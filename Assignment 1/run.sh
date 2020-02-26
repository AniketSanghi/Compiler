#!/bin/bash

check_outfile() {
    if [ ! -f "$OUTFILE" ]; then
        echo "ERROR: Outfile '$OUTFILE' is not present!"
        exit -1
    fi
}

read_input() {
    INFILE=$1

    if [ "$INFILE" = "" ]; then
        echo "ERROR: Please provide Java input file name!"
        exit -1
    fi

    #Check if given file is indeed a Java file
    T1=$(echo $INFILE | awk -F'.' '{print $1}')
    T2=$(echo $INFILE | awk -F'.' '{print $2}')
    if [ "$T2" != "java" ]; then
        echo "ERROR: File name '$INFILE' is incorrect!"
        exit -1
    fi

    #Check if file exists
    if [ ! -f $INFILE ]; then
        echo "ERROR: FIle '$INFILE' is not present!"
        exit -1
    fi

    OUTFILE="$T1.out"
}

build_code() {
    if [ ! -f lexer.lex ]; then
        echo "ERROR: Source file is not present!"
        exit -1
    fi

    flex lexer.lex
    g++ -o lexer lex.yy.c 2> /dev/null
}

######################################

# Build your code
build_code

# You need to use following variables to refer to input and output files
# You program's output must be present in OUTFILE only.
INFILE=""
OUTFILE=""

# Read input file name from user.
# And make output file name.
read_input $1

# Here you execute your lexer
./lexer < $INFILE > $OUTFILE

#In the end, check if output file is created and is not of size zero.
check_outfile

echo "Congo!! Everything seems fine so far."
