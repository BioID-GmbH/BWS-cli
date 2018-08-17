[<AutoOpen>]
module Util

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.RegularExpressions

[<Struct>] type FilePath = FilePath of string

/// Flip the order of two arguments.
let (><) f a b = f b a

/// Uncurry a function for a tuple.
let uncurry2 f (a, b) = f a b

let (|AnInt|_|) (str:string) =
    match Int32.TryParse str with
    | true, parsed -> Some parsed
    | false, _ -> None

/// Matches a URL, even if https:// needs to be added first.
let (|AnUri|_|) (str:string) =
    let withProto =
        if str.Contains "://" then str else
        "https://" + str
    match Uri.TryCreate(withProto, UriKind.Absolute) with
    | true, parsed -> Some parsed
    | false, _ -> None

/// Matches a BCID like bws.123.456 or bws/123/456.
let (|Bcid|_|) =
    let re = Regex(@"^(\w+)[./](\d+)[./](\d+)$", RegexOptions.Compiled)
    fun str ->
        let m = re.Match(str)
        if not m.Success then None else
        String.concat "." [
            for p in m.Groups |> Seq.cast<Group> |> Seq.skip 1 do
                yield p.Value
        ] |> Some

/// Matches a file that opens without error.
let (|ValidFilename|_|) path =
    try
        File.Open(path, FileMode.Append).Close()
        Some path
    with
    | _ -> None

/// Returns all files that match a glob pattern.
let glob str =
    if File.Exists str then
        str |> FilePath |> Seq.singleton
    else
        let dir = Path.GetDirectoryName str
        if String.IsNullOrEmpty dir then
            Directory.EnumerateFiles(Environment.CurrentDirectory, Path.GetFileName str)
            |> Seq.map FilePath
        elif Directory.Exists dir then
            Directory.EnumerateFiles(dir, Path.GetFileName str)
            |> Seq.map FilePath
        else
            Seq.empty

/// Used for concurrent console output.
let consoleLock = obj()

[<RequireQualifiedAccess>]
module Base64 =
    let decodeDataUrl =
        let re = Regex(@"^data:image/bmp;base64,", RegexOptions.Compiled)
        fun str ->
            let m = re.Match str
            if m.Success then
                str.Substring m.Groups.[0].Length
                |> Convert.FromBase64String
                |> Some
            else
                None

    let decode str =
        str
        |> Convert.FromBase64String
        |> Encoding.ASCII.GetString

    let urlDecode (str:string) =
        let urlUnsafe =
            str
                .Replace('-', '+')
                .Replace('_', '/')
        match urlUnsafe.Length % 4 with
        | 2 -> urlUnsafe + "=="
        | 3 -> urlUnsafe + "="
        | _ -> urlUnsafe
        |> decode

[<RequireQualifiedAccess>]
module Jwt =
    let decode (str:string) =
        str.Split('.')
        |> Array.map Base64.urlDecode

[<Sealed>]
type Debuggy() =
    static let mutable flag = true

    static do
        Debuggy.Toggle()

    static member IsDebug = flag

    static member WriteJson = false

    static member WriteLine (str) =
        if flag then
            printfn "(DEBUG) %s" str

    static member WriteLine (fmt, a) =
        if flag then
            printf "(DEBUG) "
            printfn fmt a

    static member WriteLine (fmt, a, b) =
        if flag then
            printf "(DEBUG) "
            printfn fmt a b

    [<Conditional("RELEASE")>]
    static member private Toggle () = flag <- not flag

module Print =
    /// Conditional print without newline.
    let printf print fmt = Printf.kprintf (fun str -> if print then printf "%s" str) fmt
    
    /// Conditional print with newline.
    let printfn print fmt = Printf.kprintf (fun str -> if print then printfn "%s" str) fmt
