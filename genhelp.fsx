﻿open System
open System.IO
open System.Text.RegularExpressions

type System.IO.StreamWriter with
    member me.AsyncWrite (value:string) = me.WriteAsync value |> Async.AwaitTask
    member me.AsyncWriteLine (value:string) = value + Environment.NewLine |> me.AsyncWrite

type Topic = string
type Topics = Map<Topic, string>
type Templates = Map<Topic, string list>

/// All lines entered on the console until EOF.
let lines = seq {
    let mutable loop = true
    while loop do
    match Console.ReadLine() with
    | null ->
        loop <- false
    | line ->
        yield line
}

/// Match header lines (title word followed by hyphens).
let (|Header|_|) : string -> Topic option =
    let re = Regex @"^\w+-+$"
    fun str ->
        let m = re.Match str
        if m.Success then
            Some <| str.TrimEnd '-'
        else
            None

/// Collect help topics from a list of strings.
///
/// A topic starts with a header line and continues to the next or EOF.
let rec gather : inputLines:string list -> Templates =
    let rec gather total part title = function
        | [] ->
            let rev = List.rev part
            total |> Map.add title rev
        | Header newTitle :: rest ->
            let rev = List.rev part
            gather (total |> Map.add title rev) [] newTitle rest
        | line :: rest ->
            gather total (line :: part) title rest
    gather Map.empty [] ""

let templates =
    lines
    |> Seq.toList
    |> gather

let nl = Environment.NewLine
let join = String.concat nl

/// Topics might cross-reference each other, that needs to be resolved.
/// Also join lines with newlines.
let stamped : Topics =
    templates
    |> Map.map (fun _ v ->
        let replaced =
            v
            |> List.map (fun line ->
                if line.StartsWith "%" && line.EndsWith "%" then
                    let lookup = line.Trim '%'
                    templates.[lookup] |> join
                else
                    line
            )
        join replaced
    )

/// Three quotes for raw strings.
let three = String.replicate 3 "\""

/// Four spaces for indentation.
let four = String.replicate 4 " "

printfn """// Generated by script %s, do not edit manually!

[<RequireQualifiedAccess>]
module Help

open System.Diagnostics
open System.IO

let general = %s
%s%s
""" fsi.CommandLineArgs.[0] three stamped.[""] three

printfn "let specific =%s%s[" nl four
for kv in stamped do
    assert(kv.Key.ToCharArray() |> Array.forall Char.IsLower)
    if kv.Key = "tool" then
        printfn """%s"%s", %s%s%s.Replace("#tool#", Process.GetCurrentProcess().MainModule.FileName |> Path.GetFileName)""" four kv.Key three kv.Value three
    elif kv.Key <> "" then
        printfn """%s"%s", %s%s%s""" four kv.Key three kv.Value three
printfn "%s] |> Map.ofList" four

let doc = DirectoryInfo "doc"
if not doc.Exists then
    doc.Create()

let markdownify =
    let links = Regex(@"\<(\w+)\>", RegexOptions.Compiled)
    let newlines = Regex(@":\r?\n", RegexOptions.Compiled ||| RegexOptions.Multiline)
    let examplecall = Regex(@"Example( call)?: (.*)", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    fun (str:string) ->
        let a =
            links.Replace(str, fun m ->
                sprintf "[%s](./%s.md)" m.Groups.[1].Value (m.Groups.[1].Value.ToLowerInvariant())
            )
        let b =
            newlines.Replace(a, ":  " + Environment.NewLine)
        let c = examplecall.Replace(b, "Example$1: `$2`")
        c

let toc =
    stamped
    |> Map.toList
    |> List.map fst
    |> List.filter ((<>) "")
    |> List.sort
    |> List.map (fun entry -> String.Format("- [{0}](./{0}.md)", entry))
    |> String.concat Environment.NewLine

stamped
|> Map.toList
|> List.choose (fun (key, value) ->
    if key = "" then
        Some(async {
            use md = File.Open(Path.Combine(doc.FullName, "toc.md"), FileMode.Create)
            use writer = new StreamWriter(md)
            do! writer.AsyncWriteLine "# Table of Contents"
            do! writer.AsyncWriteLine ""
            return! writer.AsyncWrite toc
        })
    elif List.contains key ["help"; "plivedetection"] then
        None
    else
        Some(async {
            use md = File.Open(Path.Combine(doc.FullName, key + ".md"), FileMode.Create)
            use writer = new StreamWriter(md)
            if key = "tool" then
                for line in value.Replace("#tool#", "bws-cli").Split '\n' do
                    let line = line.Replace("<", "&lt;").Replace(">", "&gt;").TrimEnd '\r'
                    do! writer.AsyncWrite <| line
                    if line.EndsWith(":", StringComparison.InvariantCulture) then
                        do! writer.AsyncWrite <| sprintf "  %s<PRE>" Environment.NewLine
                    elif line.StartsWith(" ", StringComparison.InvariantCulture) then
                        do! writer.AsyncWrite "  "
                    do! writer.AsyncWriteLine ""
                do! writer.AsyncWriteLine "</PRE>"
            else
                let markdown =
                    value |> markdownify
                do! writer.AsyncWriteLine markdown
            return! writer.AsyncWrite <| String.Format("{0}---{0}{0}Back to [TOC](./toc.md)", Environment.NewLine)
        })
)
|> Async.Parallel
|> Async.RunSynchronously
|> ignore
