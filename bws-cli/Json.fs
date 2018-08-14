/// Types for deserializing JSON replies.
[<RequireQualifiedAccess>]
module Json

open FSharp.Data

type Status = JsonProvider<"./json/status.json">

type EnrollAndVerify = JsonProvider<"./json/enrollverify.json", true>

type Upload = JsonProvider<"./json/upload.json", true>

type Identify = JsonProvider<"./json/identify.json", true>

type Result = JsonProvider<"./json/result.json", true>

type LivenessDetection = JsonProvider<"./json/livenessdetection.json", true>

type LiveDetection = JsonProvider<"./json/livedetection.json">

type QualityCheck = JsonProvider<"./json/qualitycheck.json", true>

type PhotoVerify = JsonProvider<"./json/photoverify.json", true>