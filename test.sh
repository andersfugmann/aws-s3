#!/bin/bash

#set -ex
#set -o pipefail

# Simple tests using the awscli
BUKCET=aws-s3-test1
TEMP=/tmp/test_data.bin
FILE=/tmp/rnd.bin
BINS="_build/install/default/bin/aws-cli-async _build/install/default/bin/aws-cli-lwt"
dd if=/dev/urandom ibs=1k count=7k of=$FILE
PART=part.bin
dd if=${FILE} of=${PART} ibs=1k skip=1 count=1k

TEST=0
function test {
    TEST=$(( TEST + 1))
    echo -n "$TEST. $1: "
    shift
    timeout 10s $@
    if [ $? -eq 0 ]; then
        echo "ok"
    else
        echo "fail"
        echo "Command: $@"
        exit 1
    fi
}

function fail {
    echo Failed: $1
    exit 1
}
function cleanup {
    rm -f ${TEMP}
    rm -f ${PART}
    rm -f ${FILE}
}

trap cleanup EXIT

function suite () {
    BIN=$1
    HTTPS=$2
    echo "TEST $(basename $BIN) https=$HTTPS"

    test "upload" ${BIN} cp --https=${HTTPS} $FILE s3://${BUKCET}/test
    test "head" ${BIN} head --https=${HTTPS} s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}


    test "upload expect" ${BIN} cp -e --https=${HTTPS} $FILE s3://${BUKCET}/test
    test "head" ${BIN} head --https=${HTTPS} s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

test "download stream" ${BIN} cp -c 8209 --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "upload chunked expect" ${BIN} cp -e -c 8209 --https=${HTTPS} $FILE s3://${BUKCET}/test
    test "download stream" ${BIN} cp -c 8209 --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "multi_upload" ${BIN} cp --https=${HTTPS} -m $FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "multi_upload chunked" ${BIN} cp -c 8209 --https=${HTTPS} -m $FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "multi_upload chunked expect " ${BIN} cp -e -c 8209 --https=${HTTPS} -m $FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "partial get" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test --first=1024 --last=1049599 ${TEMP}
    test "partial data" diff -u ${PART} ${TEMP}

    test "partial download stream" ${BIN} cp --https=${HTTPS} --first=1024 --last=1049599 s3://${BUKCET}/test ${TEMP}
    test "partial data" diff -u ${PART} ${TEMP}

    test "ls" ${BIN} ls --https=${HTTPS} ${BUKCET}
    test "rm" ${BIN} rm --https=${HTTPS} ${BUKCET} "test"
    test "upload" ${BIN} cp --https=${HTTPS} $FILE s3://${BUKCET}/test1
    test "s3 cp" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test1 s3://${BUKCET}/test2
    test "multi rm" ${BIN} rm --https=${HTTPS} ${BUKCET} "test1" "test2"
}
for HTTPS in false true; do
    for BIN in ${BINS}; do
        dune build $BIN || exit
        suite $BIN $HTTPS
    done
done
