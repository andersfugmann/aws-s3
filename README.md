= Ocaml library for accessing Amazon S3 =

This library is based on s3_cp example found at
https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/s3_cp.ml

The library uses cohttp and async for threading.

The library supports
* Fetching credentials
* Copying file from and to s3

In progress
* List files in S3
* Delete objects in S3
