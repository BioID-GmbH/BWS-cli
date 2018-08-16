module Abbreviation

[<Struct>] type Short = Short of string

[<Struct>]
type Long = Long of string
    with
        member me.String = let (Long re) = me in re

type AbbreviationList = (Short * Long) []

/// Compute the length of the common prefix.
let prefix =
    let rec prefix i (a:string) (b:string) =
        if i >= a.Length || i >= b.Length then
            i - 1
        elif a.[i] = b.[i] then
            prefix (i + 1) a b
        else
            i
    prefix 0

/// For a list of words, return tuples with the shortest possible unique abbreviation attached.
let prefixLookup array : AbbreviationList =
    let src = Array.sort array
    let re = Array.map (fun (Long str) -> str) src
    let len = re.Length
    for i = 1 to len - 1 do
        let len = prefix re.[i - 1] re.[i]
        re.[i - 1] <- re.[i - 1].Substring(0, len + 1)
    if len >= 2 then
        re.[len - 1] <- re.[len - 1].Substring(0, 1 + prefix re.[len - 2] re.[len - 1])
    for i = 1 to len - 1 do
        if re.[i - 1].StartsWith re.[i] then
            re.[i] <- src.[i].String.Substring(0, re.[i - 1].Length)
    Array.zip (Array.map Short re) src

/// Creates a lookup function for a given AbbreviationList that returns the long
/// string for each unique abbreviation.
let lookup (where:AbbreviationList) (what:string) =
    where
    |> Array.tryFind (fun (Short short, _) -> what.StartsWith short)
    |> Option.map snd
    |> Option.filter (fun (Long long) -> long.StartsWith what)

/// Words for the tool itself.
let myWords =
    "set unset pause help documentation exit expect".Split ' '
    |> Array.map Long

/// Topics that help understands additionally.
let helpWords =
    "bcid workflow tool examples authorization shortcuts".Split ' '
    |> Array.map Long

/// Words that are commands as well as token tasks.
let commonBwsWords =
    "enroll verify identify livenessdetection".Split ' '
    |> Array.map Long

/// All commands understood by the BWS.
let bwsWords =
    "token upload result isenrolled status deleteclass livedetection qualitycheck photoverify".Split ' '
    |> Array.map Long
    |> Array.append commonBwsWords

let tokenChoice =
    commonBwsWords
    |> prefixLookup
    |> lookup

let bwsCommands =
    bwsWords
    |> prefixLookup
    |> lookup

let allCommandWords =
    bwsWords
    |> Array.append myWords
    |> prefixLookup

let allCommands =
    allCommandWords
    |> lookup

let helpLookup =
    helpWords
    |> Array.append bwsWords
    |> Array.append myWords
    |> prefixLookup

let helpTopics =
    helpLookup
    |> lookup

/// Match a valid command or a list of possibilities or nothing.
let (|Command|CommandCandidates|NoCommand|) abbrev =
    match allCommands abbrev with
    | Some found -> Command found
    | None ->
        match allCommandWords
            |> Array.choose (fun (Short short, Long long) -> if short.StartsWith abbrev then Some long else None)
            with
        | [||] -> NoCommand abbrev
        | candidates -> CommandCandidates candidates

/// Match a token task or nothing.
let (|TokenChoice|_|) = tokenChoice

/// Match a valid help topic or a list of possibilities or nothing.
let (|HelpTopic|HelpCandidates|NoHelp|) abbrev =
    match helpTopics abbrev with
    | Some found -> HelpTopic found
    | None ->
        match helpLookup
            |> Array.choose (fun (Short short, Long long) -> if short.StartsWith abbrev then Some long else None)
            with
        | [||] -> NoHelp abbrev
        | candidates -> HelpCandidates candidates

/// Return the URL of the developer documentation for a given command.
let docsUrl (Long call) =
    "https://developer.bioid.com/bwsreference/web-api"
        +
        match call with
        | "" -> ""
        | "photoverify" -> "/web-photo-verify-api"
        | "qualitycheck" -> "/web-quality-check-api"
        | cmd -> sprintf "/web-%s-api" cmd
