module Api

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open FSharp.Data
open Abbreviation
open CommandLine
open ReturnCodes
open Util

[<Struct>] type Key = Key of string

[<Struct>] type Value = Value of string

let True = true.ToString() |> Value
let False = false.ToString() |> Value

/// Warn about unknown status code for a given command.
let unknown code =
    printfn "Unexpected status code: %i" code
    RetCode.BwsError

/// An environment that will collect/remember settings.
let mutable env = Map.empty<Key, Value>

let kwargse withEnv =
    let (|KwArg|_|) (str:string) =
        let pos = str.IndexOf('=')
        if pos < 0 then None else
        Some(Key <| str.Substring(0, pos), Value <| str.Substring(pos + 1))

    let (|Enabled|_|) (str:string) =
        if not <| str.EndsWith("+", StringComparison.InvariantCulture) then None else
        str.Substring(0, str.Length - 1) |> Some

    let (|Disabled|_|) (str:string) =
        if not <| str.EndsWith("-", StringComparison.InvariantCulture) then None else
        str.Substring(0, str.Length - 1) |> Some

    let possible = "face|periocular|voice"
    let rtrait =
        Regex("^" + possible + "$",
            RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    let traits =
        Regex(@"^(" + possible + ")(,(" + possible + "))+$",
            RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

    let rec kwargs = function
        | [] -> []
        | KwArg pair :: rest ->
            Some pair :: kwargs rest
        | Enabled setting :: rest ->
            Some(Key setting, True) :: kwargs rest
        | Disabled setting :: rest ->
            Some(Key setting, False) :: kwargs rest
        | TokenChoice(Long("verify" as task)) :: rest
        | TokenChoice(Long("enroll" as task)) :: rest
        | TokenChoice(Long("identify" as task)) :: rest
        | TokenChoice(Long("livenessdetection" as task)) :: rest ->
            Some(Key "task", Value task) :: kwargs rest
        | treit :: rest when treit =~ rtrait ->
            Some(Key "trait", Value treit) :: kwargs rest
        | treit :: rest when treit =~ traits ->
            Some(Key "traits", Value treit) :: kwargs rest
        | problem :: rest ->
            printfn "Cannot parse argument '%s'." problem
            None :: kwargs rest

    fun list ->
        let parsed = list |> kwargs
        if parsed |> List.exists Option.isNone then None else
        let remaining = parsed |> List.choose id
        let toAdd =
            if withEnv then
                let map = remaining |> Map.ofList
                [
                    for kv in env do
                        if map |> Map.containsKey kv.Key |> not then
                            yield kv.Key, kv.Value
                ]
            else
                []
        toAdd @ remaining |> Some

/// Parse a string list to a mapping of keys to values.
let kwargs = kwargse true

/// Warn about invalid keyword arguments for a given command.
let invalidKwArgs arg =
    printfn "Invalid or malformed keyword arguments for %s." arg
    RetCode.ParameterError

/// Gathers all requested tokens.
let tokens = ResizeArray()

/// Get the most recent token.
let lastToken () =
    if tokens.Count = 0 then
        printfn "There was no token, you need to get one first."
        []
    else
        [HttpRequestHeaders.Authorization("Bearer " + tokens.[tokens.Count - 1])]

/// Print a text response together with the status code.
let printBody code =
    let width = 80
    let topchar = String.replicate >< "_"
    let top str =
        let middle = String.length str + 2
        let left = (width - middle) / 2
        let right = width - middle - left
        printfn "%s %s %s" (topchar left) str (topchar right)
    let bottom = String.replicate width "^"
    function
        | Text text ->
            sprintf "%i Text body" code |> top
            printfn "%s\n%s" text bottom
        | _ -> ()

let toKvList = List.map (fun (k, v) -> Key k, Value v)

let fromKvList = List.map (fun (Key k, Value v) -> k, v)

/// Make an asynchronous request to a url.
let request (opts:Options) url headers query body =
    async {
        let query = fromKvList query
        let! re =
            match body with
            | None ->
                Http.AsyncRequest(url, query, headers, silentHttpErrors=true)
            | Some body ->
                Http.AsyncRequest(url, query, headers, body=body, silentHttpErrors=true)
        Debuggy.WriteLine re.ResponseUrl
        if Debuggy.WriteJson then
            match re.Body with
            | Text text ->
                let cmd =
                    let pos = url.LastIndexOf '/'
                    url.Substring(pos + 1)
                try
                    do! File.WriteAllTextAsync(cmd + ".txt", text) |> Async.AwaitTask
                with
                | :? IOException -> ()
            | _ -> ()
        if opts.PrintResponseBody then
            printBody re.StatusCode re.Body
        return re
    }

/// The handling of a enrol and verify result are similar. This is done here.
let postUpload req cmd args = async {
    match args with
    | Some query ->
        let authHeader = lastToken()
        let! r = req authHeader query None
        match handleGeneral r, r.Body with
        | Some 200, Text text ->
            let json = Json.EnrollAndVerify.Parse text
            match json.Success, json.Error with
            | true, _ ->
                printfn "%s was successful." cmd
                return RetCode.Ok
            | false, None ->
                printfn "Not recognized."
                return RetCode.OkButNo
            | false, Some error ->
                printfn "Couldn't enroll because %s." error
                return RetCode.BwsError
        | Some 400, _ ->
            printfn "No samples were uploaded."
            return RetCode.BwsError
        | Some other, _ -> return unknown other
        | None, _ -> return RetCode.BwsError
    | None -> return invalidKwArgs cmd
}

/// Print out ambigous words when an abbreviation was not unique.
let ambiguous words =
    printfn "Ambiguous abbreviation, did you mean one of"
    words |> String.concat ", " |> printfn "\t%s"

/// This handles all commands that can be entered.
let rec call (opts:Options) (remember:ResizeArray<string>) words = async {
    let printf fmt = Print.printf (not opts.Quiet) fmt
    let printfn fmt = Print.printfn (not opts.Quiet) fmt

    match words with
    | [] -> printfn "Noop"; return RetCode.Ok
    | cmd::args ->
        let recurse = call opts remember
        let basicAuth = HttpRequestHeaders.BasicAuth opts.AppId opts.Secret
        match cmd with
        | NoCommand cmd ->
            printfn "Don't know command %s." cmd
            return RetCode.CommandError
        | CommandCandidates words ->
            ambiguous words
            return RetCode.CommandError
        | Command(Long cmd) ->
            let req = request opts <| opts.Api cmd
            match cmd with
            | "status" ->
                match args with
                | [] ->
                    let! r = req [basicAuth] [] None
                    match handleGeneral r with
                    | Some 200 ->
                        printf "BWS is running fine"
                        match r.Body with
                        | Text text ->
                            let json = Json.Status.Parse text
                            printfn " (Version %O)\n%s" json.Version json.Label
                            if opts.Verbosity >= 1 && not opts.PrintResponseBody then
                                printfn "%s" text
                        | _ -> ()
                        return RetCode.Ok
                    | Some other -> return unknown other
                    | None -> return RetCode.BwsError
                | _ ->
                    printfn "%s doesn't take any arguments." cmd
                    return RetCode.ParameterError
            | "isenrolled" ->
                let args =
                    match args with
                    | [AnInt classId; treit] ->
                        Some(opts.Bcid classId, treit)
                    | [Bcid bcid; treit] ->
                        Some(bcid, treit)
                    | _ ->
                        None
                match args with
                | Some(bcid, treit) ->
                    let! r = req [basicAuth] (toKvList ["bcid", bcid; "trait", treit]) None
                    match handleGeneral r with
                    | Some 200 ->
                        printfn "User with trait is available, verification is possible."
                        return RetCode.Ok
                    | Some 400 ->
                        printfn "Invalid BCID or trait."
                        return RetCode.BwsError
                    | Some 404 ->
                        printfn "User not enrolled."
                        return RetCode.OkButNo
                    | Some other -> return unknown other
                    | None -> return RetCode.BwsError
                | _ ->
                    printfn "%s expects two arguments, a BCID and a trait." cmd
                    return RetCode.ParameterError
            | "token" ->
                let args =
                    match args with
                    | AnInt classId :: rest ->
                        Some(opts.Bcid classId, kwargs rest)
                    | Bcid bcid :: rest ->
                        Some(bcid, kwargs rest)
                    | "livenessdetection" :: rest ->
                        Some(opts.Bcid 0, kwargs rest)
                    | _ -> None
                match args with
                | Some(bcid, Some more) ->
                    if more |> List.exists (function (Key "task", Value "enroll") -> true | _ -> false) then
                        // For enrollment, we take not of the BCID.
                        remember.Add bcid
                    let! r = req [basicAuth] (toKvList ["id", opts.AppId; "bcid", bcid] @ more) None
                    match handleGeneral r, r.Body with
                    | Some 200, Text jwt ->
                        printfn "Got a token, use within 10 minutes."
                        tokens.Add jwt
                        if opts.Verbosity >= 1 then
                            let decoded = Jwt.decode jwt
                            for i = 0 to 1 do
                                printfn "%s" decoded.[i]
                        return RetCode.Ok
                    | Some 400, _ ->
                        printfn "Invalid BCID or trait."
                        return RetCode.BwsError
                    | Some other, _ -> return unknown other
                    | None, _ -> return RetCode.BwsError
                | Some(_, None) -> return invalidKwArgs cmd
                | _ ->
                    printfn "%s expects one mandatory argument bcid followed by optional keyword arguments (name=value, no spaces)." cmd
                    return RetCode.ParameterError
            | "upload" ->
                let args, files =
                    args
                    |> List.partition (glob >> Seq.isEmpty)
                match args |> kwargs with
                | Some query ->
                    let authHeader = lastToken()
                    let! uploads =
                        files
                        |> Seq.collect glob
                        |> Seq.distinct
                        |> Seq.choose mime
                        |> Seq.map (fun (mime, ((FilePath fname) as pname)) -> async {
                            let! base64 = dataUrl mime pname
                            let! r =
                                base64
                                |> TextRequest
                                |> Some
                                |> req authHeader query
                            return
                                lock consoleLock (fun () ->
                                    let name = lazy Path.GetFileName fname
                                    match handleGeneral r with
                                    | Some 200 ->
                                        printfn "'%s' uploaded." name.Value
                                        RetCode.Ok
                                    | Some 400 ->
                                        printfn "'%s' is corrupt or not supported." name.Value
                                        RetCode.BwsError
                                    | Some other -> unknown other
                                    | None -> RetCode.BwsError
                                )
                        })
                        |> Async.Parallel

                    match uploads |> Array.tryFind ((<>)RetCode.Ok) with
                    | Some code ->
                        printfn "There were errors during upload."
                        return code
                    | _ -> return RetCode.Ok
                | None -> return invalidKwArgs cmd
            | "enroll" ->
                remember.Add "."
                return! args |> kwargs |> postUpload req cmd
            | "verify" ->
                return! args |> kwargs |> postUpload req cmd
            | "identify" ->
                match args |> kwargs with
                | Some query ->
                    let authHeader = lastToken()
                    let! r = req authHeader query None
                    match handleGeneral r, r.Body with
                    | Some 200, Text text ->
                        let json = Json.Identify.Parse text
                        if json.Success then
                            printfn "Best matches:"
                            for m in json.Matches do
                                printfn "- %.17f for %s%c%i%c%i" m.Score
                                    m.Storage Options.BcidSeparator
                                    m.Partition Options.BcidSeparator
                                    m.ClassId
                            return RetCode.Ok
                        else
                            printfn "Identification unsuccessful because %s." json.Error.Value
                            return RetCode.BwsError
                    | Some 400, _ ->
                        printfn "No samples were uploaded."
                        return RetCode.BwsError
                    | Some other, _ -> return unknown other
                    | None, _ -> return RetCode.BwsError
                | None -> return invalidKwArgs cmd
            | "result" ->
                let query =
                    match args with
                    | [] ->
                        if tokens.Count = 0 then None else
                        Some tokens.[tokens.Count - 1]
                    | [token] ->
                        Some token
                    | _ ->
                        printfn "The token must be given as a continuous string."
                        None
                match query with
                | Some token ->
                    let! r = req [basicAuth] (toKvList ["access_token", token]) None
                    match handleGeneral r, r.Body with
                    | Some 200, Text text ->
                        let json = Json.Result.Parse text
                        printf "Last biometric call was "
                        if not json.Success then
                            printfn "unsuccessful because %s." json.Error.Value
                            return RetCode.BwsError
                        else
                            let lastCall = ("an " + json.Action.Value).Replace("n v", " v")
                            printfn "%s for BCID %s." lastCall json.Bcid.Value
                            if json.Matches.Length > 0 then
                                printfn "\nThe matches were:"
                                for m in json.Matches do
                                    printfn "- %.17f for %s" m.Score m.Bcid
                            return RetCode.Ok
                    | Some other, _ -> return unknown other
                    | None, _ -> return RetCode.BwsError
                | None ->
                    printfn "No token given and none available in history."
                    return RetCode.BwsError
            | "deleteclass" ->
                let bcid =
                    match args with
                    | [AnInt id] -> Some(opts.Bcid id)
                    | [Bcid bcid] -> Some bcid
                    | _ -> None
                match bcid with
                | Some bcid ->
                    let! r =
                        Http.AsyncRequest(opts.Api cmd, ["bcid", bcid], [basicAuth],
                            silentHttpErrors=true, httpMethod="DELETE"
                    )
                    Debuggy.WriteLine r.ResponseUrl
                    if opts.PrintResponseBody then
                        printBody r.StatusCode r.Body
                    return lock consoleLock (fun () ->
                        match handleGeneral r with
                        | Some 200 ->
                            printfn "BCID %s has been deleted." bcid
                            RetCode.Ok
                        | Some 400 ->
                            printfn "Unknown BCID."
                            RetCode.BwsError
                        | Some other -> unknown other
                        | None -> RetCode.BwsError
                    )
                | _ ->
                    return lock consoleLock (fun () ->
                        printfn "%s expects a single BCID to delete." cmd
                        RetCode.ParameterError
                    )
            | "livenessdetection" ->
                let authHeader = lastToken()
                let! r = req authHeader [] None
                match handleGeneral r, r.Body with
                | Some 200, Text text ->
                    let json = Json.LivenessDetection.Parse text
                    if json.Success then
                        printfn "You're alive."
                        return RetCode.Ok
                    else
                        printfn "Liveness detection failed because %s." json.Error.Value
                        return RetCode.BwsError
                | Some other, _ -> return unknown other
                | None, _ -> return RetCode.BwsError
            | "livedetection" ->
                let images =
                    args
                    |> Seq.collect glob
                    |> Seq.choose mime
                    |> Seq.filter (fst >> notWave)
                    |> Seq.toArray
                if images.Length = 2 then
                    let! dataurls =
                        images
                        |> Array.map (uncurry2 dataUrl)
                        |> Async.Parallel
                    let form =
                        dataurls
                        |> Array.mapi (fun i d -> sprintf "liveimage%i" (i + 1), d)
                        |> Array.toList
                    let! r =
                        Http.AsyncRequest(opts.Api cmd, headers=[basicAuth],
                            body=FormValues form, silentHttpErrors=true)
                    Debuggy.WriteLine r.ResponseUrl
                    if opts.PrintResponseBody then
                        printBody r.StatusCode r.Body
                    match handleGeneral r, r.Body with
                    | Some 200, Text "true" ->
                        printfn "Live person."
                        return RetCode.Ok
                    | Some 200, Text "false" ->
                        printfn "Not a live person."
                        return RetCode.OkButNo
                    | Some 400, Text text ->
                        let json = Json.LiveDetection.Parse text
                        printfn "BWS replied with error %s." json.Message
                        return RetCode.BwsError
                    | Some other, _ -> return unknown other
                    | None, _ -> return RetCode.BwsError
                else
                    printfn "Exactly two images expected."
                    return RetCode.ParameterError
            | "qualitycheck" ->
                let args, files =
                    args |> List.partition (glob >> Seq.isEmpty)
                let images =
                    files
                    |> Seq.collect glob
                    |> Seq.choose mime
                    |> Seq.filter (fst >> notWave)
                    |> Seq.toList
                match images, kwargs args with
                | [mime, path], Some query ->
                    let! dataurl = dataUrl mime path
                    let! r =
                        Http.AsyncRequest(opts.Api cmd, fromKvList query, [basicAuth],
                            body=TextRequest dataurl)
                    Debuggy.WriteLine r.ResponseUrl
                    if opts.PrintResponseBody then
                        printBody r.StatusCode r.Body
                    let showErrors (json:Json.QualityCheck.Root) =
                        for error in json.Errors do
                            printfn "- %s" error.Code
                            printfn "\t%s" error.Message
                            match error.Details with
                            | Some details ->
                                printfn "\t(%s)" details
                            | _ -> ()
                    match handleGeneral r, r.Body with
                    | Some 200, Text text ->
                        let json = Json.QualityCheck.Parse text
                        if json.Success then
                            printf "Quality check ok"
                            if json.Errors.Length > 0 then
                                printfn " but there were warnings:"
                                showErrors json
                            else
                                printfn "."
                            match json.ProcessedSample |> Option.bind Base64.decodeDataUrl with
                            | Some contents ->
                                let fname = opts.OutputFile ".bmp"
                                do! File.WriteAllBytesAsync(fname, contents) |> Async.AwaitTask
                                printfn "Wrote %s." fname
                            | None -> ()
                            return RetCode.Ok
                        else
                            printfn "Quality check failed."
                            showErrors json
                            return RetCode.BwsError
                    | Some other, _ -> return unknown other
                    | None, _ -> return RetCode.BwsError
                | _, None -> return RetCode.ParameterError
                | _ ->
                    printfn "%s expects exactly one image." cmd
                    return RetCode.ProgramError
            | "photoverify" ->
                let nofiles, files =
                    args |> List.partition (glob >> Seq.isEmpty)
                let images =
                    files
                    |> Seq.collect glob
                    |> Seq.choose mime
                    |> Seq.filter (fst >> notWave)
                    |> Seq.toArray
                match images.Length, kwargs nofiles with
                | 3, Some query ->
                    let! dataurls =
                        images
                        |> Array.map (uncurry2 dataUrl)
                        |> Async.Parallel
                    let form =
                        [
                            "idphoto", dataurls.[0]
                            "liveimage1", dataurls.[1]
                            "liveimage2", dataurls.[2]
                        ]
                    let (Value level) =
                        query
                        |> List.tryFind (fst >> ((=) (Key "accuracy")))
                        |> Option.map (snd)
                        |> Option.defaultValue (Value "4")
                    if level = "?" then
                        let globbed =
                            images
                            |> Array.map (snd)
                            |> Array.toList
                            |> List.map (fun fp ->
                                match fp with
                                | Util.FilePath path -> path
                            )
                        let levels = [5..-1..1]
                        let! sub =
                            levels
                            |> List.map (fun l -> recurse (cmd :: (sprintf "accuracy=%i" l) :: globbed))
                            |> Async.Parallel
                        match sub |> Array.tryFindIndex ((=) RetCode.Ok) with
                        | Some i ->
                            printfn "Got accepted up to level %i." levels.[i]
                            return RetCode.Ok
                        | _ ->
                            printfn "No success even with the worst level."
                            return RetCode.OkButNo
                    else
                        let! r =
                            Http.AsyncRequest(opts.Api cmd, fromKvList query, [basicAuth],
                                body=FormValues form, silentHttpErrors=true)
                        Debuggy.WriteLine r.ResponseUrl
                        if opts.PrintResponseBody then
                            printBody r.StatusCode r.Body
                        match handleGeneral r, r.Body with
                        | Some 200, Text "true" ->
                            printfn "Live images match the id photo wrt. confidence level %s." level
                            return RetCode.Ok
                        | Some 200, Text "false" ->
                            printfn "The live images don't match the id photo wrt. the demanded confidence level %s." level
                            return RetCode.OkButNo
                        | _, Text text ->
                            let json = Json.PhotoVerify.Parse text
                            printfn "BWS returned error %s." json.Message
                            return RetCode.BwsError
                        | _ -> return RetCode.BwsError
                | _, None -> return RetCode.ParameterError
                | _ ->
                    printfn "%s expects exactly 3 images, the first being from the passport." cmd
                    return RetCode.ParameterError
            | "set" ->
                match args |> kwargse false with
                | Some [] ->
                    if Map.isEmpty env then
                        printfn "No settings"
                    else
                        printfn "Settings:"
                        env |> Map.iter (fun (Key k) (Value v) -> printfn "- %s=%s" k v)
                    return RetCode.Ok
                | Some list ->
                    for key, value in list do
                        env <- env |> Map.add key value
                    return RetCode.Ok
                | None -> return invalidKwArgs cmd
            | "unset" ->
                match args with
                | [] ->
                    env <- Map.empty
                    return RetCode.Ok
                | list ->
                    for key in list do
                        env <- env |> Map.remove (Key key)
                    return RetCode.Ok
            | "pause" ->
                let ms =
                    match args with
                    | AnInt ms :: _ -> ms
                    | _ -> 500
                printfn "Sleeping for %i milliseconds..." ms
                do! Async.Sleep ms
                return RetCode.Ok
            | "documentation" ->
                match args with
                | [] ->
                    let url = docsUrl(Long "")
                    let pi = ProcessStartInfo(url, UseShellExecute=true)
                    Process.Start pi |> ignore
                    return RetCode.Ok
                | [Command command] when myWords |> Array.contains command ->
                    let (Long str) = command
                    printfn "%s is not a BWS web API command, so there's no documentation." str
                    return RetCode.ParameterError
                | [Command command] ->
                    let url = docsUrl command
                    let pi = ProcessStartInfo(url, UseShellExecute=true)
                    Process.Start pi |> ignore
                    return RetCode.Ok
                | [CommandCandidates words] ->
                    ambiguous words
                    return RetCode.ParameterError
                | [NoCommand command] ->
                    printfn "%s is not a known command." command
                    return RetCode.ParameterError
                | _ ->
                    printfn "%s expects exactly one command to get help for." cmd
                    return RetCode.ParameterError
            | "help" ->
                match args with
                | [] ->
                    printfn "%s" Help.general
                    return RetCode.Ok
                | [HelpTopic(Long topic)] ->
                    match topic with
                    | Command(Long command) ->
                        printfn "\n--- Help on %s ---\n" command
                        Help.specific |> Map.find command |> printfn "%s\n"
                    | _ ->
                        printfn "\n--- About %s ---\n" topic
                        Help.specific |> Map.find topic |> printfn "%s\n"
                    return RetCode.Ok
                | [HelpCandidates words] ->
                    ambiguous words
                    return RetCode.ParameterError
                | [NoHelp word] ->
                    printfn "Don't know anything about %s." word
                    return RetCode.ParameterError
                | _ ->
                    printfn "%s expects exactly one command to get help for." cmd
                    return RetCode.ParameterError
            | cmd ->
                printfn "Command %s no handled." cmd
                return RetCode.CommandError
    }

        