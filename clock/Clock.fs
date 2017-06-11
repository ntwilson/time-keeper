module rec TimeKeeper.Main

open System
open System.IO
open System.Diagnostics
open TimeKeeper

let runProc name (args:string[]) =
  io {
    let procInfo = ProcessStartInfo (name, String.Join (" ", args))
    procInfo.UseShellExecute <- false

    do! IO (fun () -> 
      use proc = Process.Start procInfo
      proc.WaitForExit ())
  }

let split delimiter (s:string) = s.Split [|delimiter|]
let startsWith prefix (str:string) = str.StartsWith prefix 

let readFile filename =
  io {
    let! lines = IO (fun () -> File.ReadAllLines filename)
    return lines |> Array.map (split ',')
  }

let appendToFile (line:string[]) filename =
  io {
    let rawLine = String.Join (",", line)
    do! IO (fun () -> File.AppendAllText (filename, rawLine))
  }

let createFileIfNeeded filename = 
  io {
    let! exists = IO (fun () -> File.Exists filename)
    if not exists then
      do! appendToFile [|"Clocked in:"; "Clocked out:"; "Time elapsed"|] filename 
  }

let clockedOutFromContents lines =   
  if lines |> Seq.isEmpty then 
    true
  else 
    (lines |> Seq.last |> Seq.length) > 1


let clockedOut filename = 
  io {
    let! lines = readFile filename
    return clockedOutFromContents lines
  }

let clockIn args filename = 
  io {
    let! isClockedOut = clockedOut filename
    if not isClockedOut then 
      return Result.Error "ERR: didn't do anything: you're already clocked in!"
    else
      let inTime = DateTime.Now
      do! appendToFile [|"\n" + (inTime.ToString ())|] filename
      return Result.Ok ("Clocked in at: " + inTime.ToString ())
  }

let clockOut args filename = 
  io {
    let! isClockedOut = clockedOut filename
    if isClockedOut then 
      return Result.Error "ERR: didn't do anything: you aren't clocked in!"
    else
      let outTime = DateTime.Now
      let! inTime' = inTime filename
      let elapsedTime = outTime - inTime'
      do! appendToFile [|"," + (DateTime.Now.ToString ()); (elapsedTime.ToString ())|] filename
      return Result.Ok ("Clocked out; elapsed time: " + elapsedTime.ToString ())
  }

let inTimeFromFile = 
  Seq.last >> Seq.head >> string >> Convert.ToDateTime

let inTime filename =
  io {
    let! fileContents = readFile filename
    return inTimeFromFile fileContents
  }

let openFile filename = 
  runProc "notepad++" [| filename |]

let workflow argv filename = 
  io {
    do! createFileIfNeeded filename

    return!
      match argv |> List.ofArray with
      | "in" :: args -> clockIn args filename 
      | "out" :: args -> clockOut args filename
      | "open" :: args -> 
        io { 
          do! openFile filename 
          return Result.Ok ""
        }
      | [] -> io { return Result.Error "You need to specify an argument" }
      | _ -> io { return Result.Error "huh?" }
  }

[<EntryPoint>]
let main argv = 
  let filename = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "timesheet.csv")

  let rslt = workflow argv filename |> IO.run

  let msg = 
    match rslt with
    | Result.Error err -> err
    | Result.Ok status -> status

  printfn "%s" msg
  0 
