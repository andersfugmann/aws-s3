#!/bin/bash

#set -ex
#set -o pipefail

# Simple tests using the awscli
BUKCET=aws-s3-test1
REDIRECT_BUKCET=aws-s3-test-eu
TEMP=/tmp/test_data.bin

LARGE_FILE=/tmp/rnd_big.bin
FILE=/tmp/rnd.bin
dd if=/dev/urandom of=$LARGE_FILE ibs=1k count=17k
dd if=$LARGE_FILE  of=$FILE       ibs=1k count=129

FIRST_PART=1000
LAST_PART=68000
PART=/tmp/part.bin
dd if=${LARGE_FILE} of=${PART} ibs=1 skip=$(( FIRST_PART )) count=$(( LAST_PART - FIRST_PART + 1))

TEST=0
function test {
    TEST=$(( TEST + 1))
    echo -n "$TEST. $1: "
    shift
    $@
    if [ $? -eq 0 ]; then
        echo "ok"
    else
        echo "fail"
        echo "Command: $@"
        exit 1
    fi
}

function cleanup {
    rm -f ${TEMP}
    rm -f ${PART}
    rm -f ${FILE}
}

#trap cleanup EXIT
function test_simple () {
    BIN=$1;shift
    RETRIES=$1;shift
    HTTPS=$1;shift

    echo "TEST SIMPLE $(basename $BIN) HTTPS=${HTTPS}"
    test "upload" ${BIN} cp --https=${HTTPS} --retries=${RETRIES} $FILE s3://${BUKCET}/test
    test "head" ${BIN} head --https=${HTTPS} --retries=${RETRIES} s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}
}

function test_complete () {
    BIN=$1;shift
    RETRIES=$1;shift
    echo "TEST $(basename $BIN)"

    #test "redirect upload expect" ${BIN} cp -e --retries=${RETRIES} $FILE s3://${REDIRECT_BUKCET}/test
    #test "redirect head" ${BIN} head --retries=${RETRIES} s3://${REDIRECT_BUKCET}/test
    #test "redirect download" ${BIN} cp --retries=${RETRIES} s3://${REDIRECT_BUKCET}/test ${TEMP}
    #test "redirect data" diff -u $FILE ${TEMP}

    test "upload expect" ${BIN} cp -e --retries=${RETRIES} $FILE s3://${BUKCET}/test
    test "head" ${BIN} head --retries=${RETRIES} s3://${BUKCET}/test
    test "download" ${BIN} cp --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "download stream" ${BIN} cp -c 8209 --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "upload chunked expect" ${BIN} cp -e -c 8209 --retries=${RETRIES} $FILE s3://${BUKCET}/test
    test "download stream" ${BIN} cp -c 8209 --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "multi_upload" ${BIN} cp --retries=${RETRIES} -m $LARGE_FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $LARGE_FILE ${TEMP}

    test "multi_upload chunked" ${BIN} cp -c 8209 --retries=${RETRIES} -m $LARGE_FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $LARGE_FILE ${TEMP}

    test "multi_upload chunked expect" ${BIN} cp -e -c 8209 --retries=${RETRIES} -m $LARGE_FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --retries=${RETRIES} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $LARGE_FILE ${TEMP}

    test "partial download" ${BIN} cp --retries=${RETRIES} s3://${BUKCET}/test --first=$FIRST_PART --last=$LAST_PART ${TEMP}
    test "partial data" diff -u ${PART} ${TEMP}

    test "partial download stream" ${BIN} cp --retries=${RETRIES} --first=$FIRST_PART --last=$LAST_PART s3://${BUKCET}/test ${TEMP}
    test "partial data" diff -u ${PART} ${TEMP}

    test "rm" ${BIN} rm --retries=${RETRIES} ${BUKCET} "test"
    test "upload" ${BIN} cp --retries=${RETRIES} $FILE s3://${BUKCET}/test1
    test "s3 cp" ${BIN} cp --retries=${RETRIES} s3://${BUKCET}/test1 s3://${BUKCET}/test2
    test "ls" ${BIN} ls --max-keys=1 --retries=${RETRIES} ${BUKCET}
    test "multi rm" ${BIN} rm --retries=${RETRIES} ${BUKCET} "test1" "test2"
}

# Test complete functionality for both lwt and async
for b in lwt async; do
    BIN=_build/install/default/bin/aws-cli-$b
    dune build $BIN || exit
    test_simple $BIN 3 false
    test_simple $BIN 3 true
    test_complete $BIN 3
done
