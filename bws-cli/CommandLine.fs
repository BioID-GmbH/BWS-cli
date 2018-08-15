module CommandLine

open System
open System.IO

type Options = {
    Url:Uri
    Partition:int
    Storage:string
    AppId:string
    Secret:string
    Output:string option
    Verbosity:int
    Interactive:bool
    PrintResponseBody:bool
    CleanUp:bool
    Quiet:bool
    Help:bool
} with
    static member Default = {
        Url = Uri("https://bws.bioid.com/extension")
        Partition = 112
        Storage = "bws"
        AppId = ""
        Secret = ""
        Output = None
        Verbosity = 0
        Interactive = false
        PrintResponseBody = false
        CleanUp = false
        Quiet = false
        Help = false
    }

    /// Get the URL for a given command.
    member me.Api (right:string) =
        let left = me.Url.AbsoluteUri
        match left.[left.Length - 1], right.[0] with
        | '/', '/' ->
            left + right.Substring 1
        | '/', _
        | _, '/' ->
            left + right
        | _ ->
            String.Join("/", left, right)

    /// Construct a BCID from a number by combining it with the storage and partition.
    member me.Bcid classId =
        let re =
            sprintf "%s%c%i%c%i"
                me.Storage Options.BcidSeparator me.Partition Options.BcidSeparator classId
        if me.Verbosity >= 1 then
            printfn "Using BCID %s" re
        re

    /// Get a random output filename.
    member me.OutputFile () =
        match me.Output with
        | Some path -> path
        | None -> Path.GetRandomFileName()

    /// Get a random output filename with a given extension.
    member me.OutputFile ext =
        let path = me.OutputFile()
        let replace = Path.GetExtension path
        path.Replace(replace, ext)

    /// Separates the three parts of a BCID.
    static member BcidSeparator = '.'

/// Match a single dash option (like -v) and also provide the option stacked with the one in question removed.
let (|SingleDash|_|) (chr:char) (str:string) =
    if str.StartsWith "--" then None else 
    if not (str.StartsWith "-") then None else
    let i = str.IndexOf(chr, 1)
    if i >= 1 then
        str.Substring(0, i) + str.Substring(i + 1) |> Some
    else
        None

/// Match an arguments that does not begin with a dash.
let (|NoDash|_|) (str:string) =
    if str.StartsWith "-" then None else
    Some str

/// Parse the command line options and turn them into a data structure.
let parse =
    let rec parse (acc, args) = function
        | [] ->
            acc, List.rev args
        | "-" :: rest ->
            // Eating chars from a one dash pack leaves a useless - in the end.
            parse (acc, args) rest
        | "--endpoint" :: AnUri url :: rest ->
            rest |> parse ({ acc with Url = url }, args)
        | SingleDash 'e' r :: AnUri url :: rest ->
            r :: rest |> parse ({ acc with Url = url }, args)
        | "--partition" :: AnInt num :: rest ->
            rest |> parse ({ acc with Partition = num }, args)
        | SingleDash 'p' r :: AnInt num :: rest ->
            r :: rest |> parse ({ acc with Partition = num }, args)
        | "--storage" :: where :: rest ->
            rest |> parse ({ acc with Storage = where }, args)
        | SingleDash 's' r :: where :: rest ->
            r :: rest |> parse ({ acc with Storage = where }, args)
        | "--app" :: NoDash id :: rest ->
            rest |> parse ({ acc with AppId = id }, args)
        | SingleDash 'a' r :: NoDash id :: rest ->
            r :: rest |> parse ({ acc with AppId = id }, args)
        | "--password" :: secret :: rest ->
            rest |> parse ({ acc with Secret = secret }, args)
        | SingleDash 'w' r :: secret :: rest ->
            r :: rest |> parse ({ acc with Secret = secret }, args)
        | "--verbose" :: rest ->
            rest |> parse ({ acc with Verbosity = acc.Verbosity + 1 }, args)
        | SingleDash 'v' r :: rest ->
            r :: rest |> parse ({ acc with Verbosity = acc.Verbosity + 1 }, args)
        | "--interactive" :: rest ->
            rest |> parse ({ acc with Interactive = true }, args)
        | SingleDash 'i' r :: rest ->
            r :: rest |> parse ({ acc with Interactive = true }, args)
        | "--print-body" :: rest ->
            rest |> parse ({ acc with PrintResponseBody = true }, args)
        | SingleDash 'b' r :: rest ->
            r :: rest |> parse ({ acc with PrintResponseBody = true }, args)
        | "--clean-up" :: rest ->
            rest |> parse ({ acc with CleanUp = true }, args)
        | SingleDash 'c' r :: rest ->
            r :: rest |> parse ({ acc with CleanUp = true }, args)
        | "--quiet" :: rest ->
            rest |> parse ({ acc with Quiet = true }, args)
        | SingleDash 'q' r :: rest ->
            r :: rest |> parse ({ acc with Quiet = true }, args)
        | "--output" :: ValidFilename path :: rest ->
            rest |> parse ({ acc with Output = Some path }, args)
        | SingleDash 'o' r :: ValidFilename path :: rest ->
            r :: rest |> parse ({ acc with Output = Some path }, args)
        | "--help" :: _
        | SingleDash 'h' _ :: _
        | SingleDash '?' _ :: _ ->
            { acc with Help = true }, []
        | NoDash arg :: rest ->
            parse (acc, arg :: args) rest
        | problem :: _ ->
            failwithf "Problem with parameter: %s" problem
    parse (Options.Default, [])
