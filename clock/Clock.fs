open System
open System.IO
open System.Diagnostics

let runProc name (args:string[]) =
  let procInfo = ProcessStartInfo (name, String.Join (" ", args))
  procInfo.UseShellExecute <- false

  let proc = Process.Start procInfo
  proc.WaitForExit ()
  proc.Close ()

let readFile filename =
  File.ReadAllLines filename

let startsWith prefix (str:string) = str.StartsWith prefix 

let clockedOut filename = 
  let lines = readFile filename
  lines |> Seq.isEmpty
    || lines |> Seq.last |> startsWith "Clocked out:"

let appendToFile line filename =
  File.AppendAllLines (filename, [line])

let clockIn args filename = 
  if not (clockedOut filename) then Result.Error "ERR: didn't do anything: you're already clocked in!"
  else
    appendToFile ("\n\n--------------\nClocked in: " + (DateTime.Now.ToString ())) filename
    Result.Ok "Clocked in!"

let clockOut args filename = 
  if clockedOut filename then Result.Error "ERR: didn't do anything: you aren't clocked in!"
  else
    appendToFile ("Clocked out: " + (DateTime.Now.ToString ())) filename
    Result.Ok "Clocked out!"

let openFile filename = 
  runProc "notepad++" [| filename |]

[<EntryPoint>]
let main argv = 
  let filename = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "timesheet.txt")
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
