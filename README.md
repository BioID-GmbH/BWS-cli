# BWS-CLI

[BioID GmbH][bioid] provides a biometric web service *made in Germany*. To use our
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
[registered for a trial instance][trial]. Once you got the application running, look
around with the [help command][help].

## Code

The code is written in F# for .NET Core and depends only on the
[awesome type providers from FSharp.Data][types].

### Build

If you're on Windows or Mac and have Visual Studio installed you can use the SLN file to
build and debug.

Without Visual Studio, from the command line, you can for instance

- `dotnet run --project bws-cli`
- `dotnet build` followed by `dotnet run bws-cli\bin\Release\netcoreapp2.1\bws-cli.dll`
- `dotnet publish -c Release --runtime win-x86` to get a 32-bit EXE.

For other runtime targets see [this list][runtimes].

### Peculiarity with the produced binaries

When publishing for a given platform, you get a binary in the top level of the output
directory. Inside the `publish` subdirectory there is an identical binary. With certain
configurations you get the following error when executing the top level binary:

    Error:
      An assembly specified in the application dependencies manifest (bws-cli.deps.json) was not found:
        package: 'FSharp.Core', version: '4.5.2'
        path: 'lib/netstandard1.6/FSharp.Core.dll'

Then run the binary from the subdirectory instead, it should work.

[bioid]: https://www.bioid.com/
[bws]: https://www.bioid.com/bioid-web-service/
[api]: https://developer.bioid.com/bwsreference/web-api
[trial]: https://bwsportal.bioid.com/register
[types]: http://fsharp.github.io/FSharp.Data/library/JsonProvider.html
[runtimes]: https://docs.microsoft.com/en-us/dotnet/core/rid-catalog
[help]: ./doc/toc.md
