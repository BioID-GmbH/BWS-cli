module Program

open System
open System.Text.RegularExpressions
open Abbreviation
open Api
open CommandLine
open ReturnCodes
open Util

/// Split a string/line into words, eating all whitespace.
let split =
    let re = Regex(@"\s+", RegexOptions.Compiled)
    fun str ->
        re.Split str
        |> Array.filter (String.length >> (>) >< 0)
        |> Array.toList

/// Filter comments and empty lines.
let notComments : words:string list -> bool = function
    | first :: _ when not <| first.StartsWith "#" -> true
    | _ -> false

[<EntryPoint>]
let main argv =
    let opts, args =
        argv
        |> Array.toList
        |> CommandLine.parse

    if opts.AppId = Options.Default.AppId || opts.Secret = Options.Default.Secret then
        eprintfn "\nERROR: Both application id and secret need to be passed to connect to BWS!\n"
        Help.specific.["tool"] |> eprintfn "%s"
        int RetCode.Unauthorized
    else
        let args, opts =
            if List.isEmpty args then
                // I there are no commands via the command line, we switch to interactive.
                "", {opts with Interactive = true }
            else
                // Assemble words back into a line while transforming to lower.
                args
                |> List.map (fun str -> str.ToLowerInvariant())
                |> String.concat " ", opts

        let printf fmt = Print.printf (not opts.Quiet) fmt
        let printfn fmt = Print.printfn (not opts.Quiet) fmt

        let input =
            let lead = printf ">>> %s"
            let rec consoleLines before = seq {
                if before <> "" then
                    // before is the line that was passed via the command line.
                    lead before
                    printfn ""
                    yield before
                lead ""
                match Console.ReadLine() with
                | null -> ()
                | line ->
                    match line.Trim().ToLowerInvariant() with
                    | Command(Long "exit")
                    | "quit" ->
                        printfn "Bye!"
                    | line ->
                        yield line
                        yield! consoleLines ""
            }
            (
                if opts.Interactive then
                    printfn "--- Interactive BWS console"
                    consoleLines args
                else
                    [args] :> _
            )
            // Split a multi-line string into lines.
            |> Seq.map split
            // We neither want to process empty lines...
            |> Seq.filter (List.isEmpty >> not)
            // not comment lines.
            |> Seq.filter notComments
            // Rewrite these four commands with file arguments to upload command followed by the original one.
            |> Seq.collect (function
                | Command(Long("verify" as cmd)) :: files
                | Command(Long("enroll" as cmd)) :: files
                | Command(Long("identify" as cmd)) :: files
                | Command(Long("livenessdetection" as cmd)) :: files ->
                    [
                        let args, files =
                            files
                            |> List.partition (glob >> Seq.isEmpty)
                        yield "upload" :: files
                        yield cmd :: args
                    ]
                | unchanged -> [unchanged]
            )

        /// Remember all created BCIDs, so that we can remove them later.
        let bcids = ResizeArray<string>()

        try
            let bwsResults =
                input
                // This is the meat of the program.
                |> Seq.map (Api.call opts bcids)
                // We cannot mix or overlap commands, because they probably depend on each other.
                |> Seq.map Async.RunSynchronously
            
            let combine (source:seq<_>) = seq {
                use enm = source.GetEnumerator()
                let mutable prev = None
                let mutable stop = false
                while enm.MoveNext() && not stop do
                    match prev, enm.Current with
                    | Some (Return code), Expect exp ->
                        if code = exp then
                            printfn "Expectation matched."
                            yield RetCode.Ok
                        else
                            printf "Expected %O but got %O" exp code
                            if opts.Interactive then
                                printfn "."
                            else
                                printfn ", exiting..."
                            stop <- true
                            yield code
                    | Some (Return code), Return _ ->
                        yield code
                    | _ -> ()
                    prev <- Some enm.Current
                match stop, prev with
                | false, Some (Return code) -> yield code
                | _ -> ()
            }

            let combined = bwsResults |> combine

            // We can only return a single error code. I decided for the first one.
            let firstBad =
                combined
                // Why list? Because I want to evaluate everything, but Seq is lazy.
                |> Seq.toList
                |> List.tryFind ((<>)RetCode.Ok)
            
            match firstBad with
            | Some code ->
                Debuggy.WriteLine("Exiting with %O (%i).", code, int code)
                int code
            | None ->
                Debuggy.WriteLine("Exiting normally.")
                0
        finally
            // Extract all BCIDs that were enrolled (without the dot, the token didn't go through).
            let rec createdBcids i = seq {
                if i >= 1 then
                    if bcids.[i] = "." && bcids.[i - 1] <> "." then
                        yield bcids.[i - 1]
                        yield! createdBcids (i - 2)
                    else
                        yield! createdBcids (i - 1)
                }

            if opts.CleanUp then
                bcids.Count - 1
                |> createdBcids
                |> Seq.toList
                |> Seq.distinct
                |> Seq.map (fun id -> Api.call { opts with Verbosity = 0; PrintResponseBody = false } bcids ["deleteclass"; id])
                |> Async.Parallel
                |> Async.Ignore
                |> Async.RunSynchronously
