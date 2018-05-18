# Ocaml library for accessing Amazon S3

This library provides access to Amazon Simple Storage Solution (S3).

The library supports:
* Fetching credentials (though IAM)
* Copying file from and to s3
* List files in S3 (from root)
* Delete single object in S3

The library supports both lwt and async concurrency models.
* For lwt, please install `aws-s3-lwt` package
* For Async, please install `aws-s3-async` package

[Api](https://andersfugmann.github.io/aws-s3/)

This library is based on s3_cp example found at
[https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/s3_cp.ml]
