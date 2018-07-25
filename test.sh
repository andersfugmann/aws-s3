#!/bin/bash

#set -ex
#set -o pipefail

# Simple tests using the awscli
BUKCET=aws-s3-test1
TEMP=$(mktemp)
BIN="_build/install/default/bin/aws-cli-lwt"
dune build $BIN

FILE=/tmp/aaas
dd if=/dev/zero ibs=1k count=65 | tr "\000" "A" > $FILE

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

test "upload" ${BIN} cp $FILE s3://${BUKCET}/test
test "head" ${BIN} head s3://${BUKCET}/test
#| grep -q ${MD5}"
test "download" ${BIN} cp s3://${BUKCET}/test ${TEMP}
test "data" diff -u $FILE ${TEMP}

test "multi_upload" ${BIN} cp -m $FILE s3://${BUKCET}/test
test "download" ${BIN} cp s3://${BUKCET}/test ${TEMP}
test "data" diff -u $FILE ${TEMP}

test "partial get" ${BIN} cp --first=10 --last=19 s3://${BUKCET}/test ${TEMP}
test "partial result" [ "$(wc -c ${TEMP} | cut -f 1 -d' ')" -eq "10" ]

test "ls" ${BIN} ls ${BUKCET}
test "rm" ${BIN} rm ${BUKCET} "test"
test "upload" ${BIN} cp $FILE s3://${BUKCET}/test1
test "upload" ${BIN} cp $FILE s3://${BUKCET}/test2
test "multi rm" ${BIN} rm ${BUKCET} "test1" "test2"
