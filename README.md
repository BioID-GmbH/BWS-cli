# BWS-CLI

[BioID GmbH][] provides a biometric web service *made in Germany*. To use our
*privacy-enabled* [BWS][], your can resort to SOAP or a web API.

This command line interface is both a test client as well as a demo for all
[HTTP API][api] calls. With it you can, among other things,

1. Connect to the BWS
2. Check the BWS status
3. Enroll users
4. Verify/identify enrolled users
5. Liveness detection
6. Do quality checks or passport verifications

## What you need

To start the CLI you an application ID and secret, which you can get after having
[registered for a trial instance][trial].

## Code

The code is written in F# for .NET Core and depends only on the
[awesome type providers from FSharp.Data][types].

[bioid]: https://www.bioid.com/
[bws]: https://www.bioid.com/bioid-web-service/
[api]: https://developer.bioid.com/bwsreference/web-api
[trial]: https://bwsportal.bioid.com/register
[types]: http://fsharp.github.io/FSharp.Data/library/JsonProvider.html
