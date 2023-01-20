# Ocaml library for accessing Amazon S3

[![Main workflow](https://github.com/andersfugmann/aws-s3/actions/workflows/workflow.yml/badge.svg)](https://github.com/andersfugmann/aws-s3/actions/workflows/workflow.yml)


This library provides access to Amazon Simple Storage Solution (S3).

The following S3 operations are supported:
* Copying file to and from s3
* List files in S3 (from root)
* Delete single/multi object in S3
* HEAD operation on single objects
* Streaming transfer to and from aws
* Multi part upload (including s3 -> s3 copy)

The library also implements fetching credentials through IAM service.

The library supports both lwt and async concurrency models.
* For lwt, please install `aws-s3-lwt` package
* For Async, please install `aws-s3-async` package

[Api](https://andersfugmann.github.io/aws-s3/)

This library is originally based on [s3_cp.ml](https://raw.githubusercontent.com/mirage/ocaml-cohttp/5c3d77cde632f366bfdf9521b95648527174a2f3/examples/async/s3_cp.ml) from the mirage project but has diverted (and grown) substantially since.
