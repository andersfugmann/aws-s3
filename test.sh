#!/bin/bash

#set -ex
#set -o pipefail

# Simple tests using the awscli
BUKCET=aws-s3-test1
TEMP=$(mktemp)
BIN="jbuilder exec aws-cli-async --"

function fail {
    echo Failed: $1
    exit 1
}
function cleanup {
    rm -f ${TEMP}
}
trap cleanup EXIT

echo "Test Simple put"
${BIN} cp $0 s3://${BUKCET}/test || fail "upload"
${BIN} cp s3://${BUKCET}/test ${TEMP} || fail "download"
diff $0 ${TEMP} || "wrong data"


echo "Test Multi put"
${BIN} cp -m $0 s3://${BUKCET}/test || fail "multi_upload"
${BIN} cp s3://${BUKCET}/test ${TEMP} || fail "download"
diff $0 ${TEMP} || "wrong data"


echo "Test Partial get"
${BIN} cp $0 s3://${BUKCET}/test
${BIN} cp --first=10 --last=19 s3://${BUKCET}/test ${TEMP}
[ "$(wc -c ${TEMP} | cut -f 1 -d' ')" -eq "10" ]

echo "Test ls"
MD5=$(md5sum $0 | cut -f 1 -d' ')
${BIN} ls ${BUKCET} | grep -q "${MD5}" || fail "ls"

echo "Test head"
${BIN} head s3://${BUKCET}/test | grep -q "${MD5}" || fail "head"

echo "Test rm"
${BIN} rm ${BUKCET} test || fail "rm"
${BIN} ls ${BUKCET} > /dev/null || fail "ls"
echo All OK
