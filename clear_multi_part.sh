#!/bin/bash
BUCKET=$1
aws s3api list-multipart-uploads --bucket $BUCKET |
    jq -r '.Uploads[] | "\(.UploadId) \(.Key)"' |
    while read id key; do
        echo KEY: $key, ID $id
        aws s3api abort-multipart-upload --bucket $BUCKET --key $key --upload-id $id
    done
