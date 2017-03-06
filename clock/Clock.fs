module rec TimeKeeper

open System
open System.IO
open System.Diagnostics

let runProc name (args:string[]) =
  let procInfo = ProcessStartInfo (name, String.Join (" ", args))
  procInfo.UseShellExecute <- false

  let proc = Process.Start procInfo
  proc.WaitForExit ()
  proc.Close ()

let split delimiter (s:string) = s.Split [|delimiter|]
let startsWith prefix (str:string) = str.StartsWith prefix 

let readFile filename =
  File.ReadAllLines filename
  |> Array.map (split ',')

let appendToFile (line:string[]) filename =
  let rawLine = String.Join (",", line)
  File.AppendAllText (filename, rawLine)

let clockedOut filename = 
  if not (File.Exists filename) then appendToFile [|"Clocked in:"; "Clocked out:"; "Time elapsed"|] filename
  let lines = readFile filename

  if lines |> Seq.isEmpty then true
  else (lines |> Seq.last |> Seq.length) > 1

let clockIn args filename = 
  if not (clockedOut filename) then Result.Error "ERR: didn't do anything: you're already clocked in!"
  else
    let inTime = DateTime.Now
    appendToFile [|"\n" + (inTime.ToString ())|] filename
    Result.Ok ("Clocked in at: " + inTime.ToString ())

let clockOut args filename = 
  if clockedOut filename then Result.Error "ERR: didn't do anything: you aren't clocked in!"
  else
    let outTime = DateTime.Now
    let elapsedTime = outTime - (inTime filename)
    appendToFile [|"," + (DateTime.Now.ToString ()); (elapsedTime.ToString ())|] filename
    Result.Ok ("Clocked out; elapsed time: " + elapsedTime.ToString ())

let inTime filename =
  let inStr = 
    readFile filename
    |> Seq.last |> Seq.head
  
  Convert.ToDateTime inStr

let openFile filename = 
  runProc @"C:\Program Files (x86)\Microsoft Office\Office15\excel" [| filename |]

[<EntryPoint>]
let main argv = 
  let filename = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "timesheet.csv")
  let rslt = 
    match argv |> List.ofArray with
    | "in" :: args -> clockIn args filename 
    | "out" :: args -> clockOut args filename
    | "open" :: args -> openFile filename; Result.Ok ""
    | [] -> Result.Error "You need to specify an argument"
    | _ -> Result.Error "huh?"

  let msg = 
    match rslt with
    | Result.Error err -> err
    | Result.Ok status -> status

  printfn "%s" msg
  0 
