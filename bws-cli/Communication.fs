module ReturnCodes

open System
open System.IO
open FSharp.Data
open Util

[<Struct>] type Mime = Mime of string

type RetCode =
    | Ok = 0
    | OkButNo = 1
    | Unauthorized = 2
    | CommandError = 3
    | ParameterError = 4
    | BwsError = 5

/// Some commands always mean roughly the same. Let's handle them here.
let handleGeneral (r:HttpResponse) =
    match r.StatusCode with
    | 401 ->
        printfn "No or invalid authentication header (basic or JWT)."
        Choice2Of2 RetCode.Unauthorized
    | 403 ->
        printfn "Access denied (wrong/expired token or password)."
        Choice2Of2 RetCode.Unauthorized
    | 500 ->
        printfn "Internal server error."
        Choice2Of2 RetCode.BwsError
    | id ->
        Choice1Of2 id

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
