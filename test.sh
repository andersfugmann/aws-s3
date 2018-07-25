#!/bin/bash

#set -ex
#set -o pipefail

# Simple tests using the awscli
BUKCET=aws-s3-test1
TEMP=$(mktemp)
BINS="_build/install/default/bin/aws-cli-async _build/install/default/bin/aws-cli-lwt"

FILE=/tmp/aaas
dd if=/dev/urandom ibs=1k count=7k of=$FILE

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
}
trap cleanup EXIT

MD5=$(md5sum $FILE | cut -f 1 -d' ')

function suite () {
    BIN=$1
    HTTPS=$2
    echo "TEST $(basename $BIN) https=$HTTPS"

    test "upload" ${BIN} cp --https=${HTTPS} $FILE s3://${BUKCET}/test
    test "head" ${BIN} head --https=${HTTPS} s3://${BUKCET}/test
    #| grep -q ${MD5}"
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "multi_upload" ${BIN} cp --https=${HTTPS} -m $FILE s3://${BUKCET}/test
    test "download stream" ${BIN} cp -c 8209 --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "multi_upload chunked" ${BIN} cp -c 8209 --https=${HTTPS} -m $FILE s3://${BUKCET}/test
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "partial get" ${BIN} cp --https=${HTTPS} --first=10 --last=19 s3://${BUKCET}/test ${TEMP}
    test "partial result" [ "$(wc -c ${TEMP} | cut -f 1 -d' ')" -eq "10" ]

    test "ls" ${BIN} ls --https=${HTTPS} ${BUKCET}
    test "rm" ${BIN} rm --https=${HTTPS} ${BUKCET} "test"
    test "upload chunked" ${BIN} cp -c 8209 --https=${HTTPS} $FILE s3://${BUKCET}/test1
    test "download" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test1 ${TEMP}
    test "data" diff -u $FILE ${TEMP}

    test "s3 cp" ${BIN} cp --https=${HTTPS} s3://${BUKCET}/test1 s3://${BUKCET}/test2
    test "multi rm" ${BIN} rm --https=${HTTPS} ${BUKCET} "test1" "test2"
}

for BIN in ${BINS}; do
    dune build $BIN
    suite $BIN false
    suite $BIN true
done
