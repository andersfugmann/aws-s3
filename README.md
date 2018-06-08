# Ocaml library for accessing Amazon S3

This library provides access to Amazon Simple Storage Solution (S3).

The following S3 operations are supported:
* Get (copy a file to s3)
* Put (Get a file from s3)
* Delete (Delete a file from s3)
* Multipart upload (Including copy)
* Multi delete
* Head suport
* Ls

The library also implements fetching credentials through IAM service.


The library supports both lwt and async concurrency models.
* For lwt, please install `aws-s3-lwt` package
* For Async, please install `aws-s3-async` package

[Api](https://andersfugmann.github.io/aws-s3/)

This library is based on s3_cp example found at
https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/s3_cp.ml
