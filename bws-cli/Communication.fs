module ReturnCodes

open System
open System.IO
open FSharp.Data
open Util

[<Struct>] type Mime = Mime of string

type RetCode =
    | Ok = 0
    | OkButNo = 1
    | DidntExpect = 2
    | ProgramError = 3
    | BwsError = 4
    | CommandError = 5
    | ParameterError = 6

/// Some commands always mean roughly the same. Let's handle them here.
let handleGeneral (r:HttpResponse) =
    match r.StatusCode with
    | 401 ->
        printfn "No or invalid authentication header (basic or JWT)."
        None
    | 403 ->
        printfn "Access denied (wrong/expired token or password)."
        None
    | 500 ->
        printfn "Internal server error."
        None
    | id ->
        Some id

/// Get a (MIME, path) tuple from the extension.
let mime pname =
    let (FilePath fname) = pname
    let last = fname.LastIndexOf '.'
    match fname.Substring(last + 1).ToLowerInvariant() with
    | "wav" -> Some(Mime "audio/wav", pname)
    | "bmp" -> Some(Mime "image/bmp", pname)
    | "png" -> Some(Mime "image/png", pname)
    | "gif" -> Some(Mime "image/gif", pname)
    | "jpg"
    | "jpeg" -> Some(Mime "image/jpeg", pname)
    | "tif"
    | "tiff" -> Some(Mime "image/tiff", pname)
    | _ ->
        if Debuggy.IsDebug then
            // For debugging, I want to be able to send corrupt files.
            // With this, I can just give a DLL or something and it is interpreted as a corrupt PNG.
            Some(Mime "image/png", pname)
        else
            None

/// Predicate for everything but wave, i. e. images.
let notWave = (<>) (Mime "audio/wav")

/// Read a binary file and turn it into a data URL.
let dataUrl (Mime mime) (FilePath file) = async {
    let! bytes =
        file
        |> File.ReadAllBytesAsync
        |> Async.AwaitTask
    let base64 = bytes |> Convert.ToBase64String
    return String.concat "" ["data:"; mime; ";base64,"; base64]
}
