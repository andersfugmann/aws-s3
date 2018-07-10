#!/bin/bash

#set -ex
#set -o pipefail

# Simple tests using the awscli
BUKCET=aws-s3-test1
TEMP=$(mktemp)
BIN="jbuilder exec aws-cli-async --"

function test {
    echo -n "$1: "
    shift
    $@
    if [ $? -eq 0 ]; then
        echo "ok"
    else
        echo "fail"
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

test "upload1" ${BIN} cp $0 s3://${BUKCET}/test
test "download1" ${BIN} cp s3://${BUKCET}/test ${TEMP}
test "wrong data1" diff $0 ${TEMP}

test "multi_upload" ${BIN} cp -m $0 s3://${BUKCET}/test
test "download" ${BIN} cp s3://${BUKCET}/test ${TEMP}
test "wrong data" diff $0 ${TEMP}

test "partial get" ${BIN} cp --first=10 --last=19 s3://${BUKCET}/test ${TEMP}
test "partial result" [ "$(wc -c ${TEMP} | cut -f 1 -d' ')" -eq "10" ]

MD5=$(md5sum $0 | cut -f 1 -d' ')
test "ls" ${BIN} ls ${BUKCET} | grep -q "${MD5}"

test "head" ${BIN} head s3://${BUKCET}/test | grep -q "${MD5}"

test "rm" ${BIN} rm ${BUKCET} test
test "ls" ${BIN} ls ${BUKCET} > /dev/null
