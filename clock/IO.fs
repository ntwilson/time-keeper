namespace TimeKeeper

type IO<'a> = IO of (unit -> 'a)

module IO = 
  let run (IO f) = f ()
  let make f a = IO (fun () -> f a)
  let make2 f a b = make (f a) b
  let make3 f a b c = make (f a b) c
  let make4 f a b c d = make (f a b c) d
  let make5 f a b c d e = make (f a b c d) e


type IOExpression() =
  member this.Bind (f, cexpr) =     
    let evaluate () =
      IO.run f
      |> cexpr
      |> IO.run

    IO evaluate

  member this.Return x = IO (fun () -> x)

  member this.ReturnFrom x = x

  member this.TryFinally (body, finish) =
    try
      this.ReturnFrom (body ())
    finally 
      finish ()

  member this.Using (disp : #System.IDisposable, cexpr) =
    let evaluate () = cexpr disp
    this.TryFinally (evaluate, 
      fun () -> 
        match disp with
        | null -> ()
        | _ -> disp.Dispose ())

  member this.Zero () = this.Return ()

  member this.Combine (f, g) =
    this.Bind (f, fun () -> g)

  member this.Delay f = f
  member this.Run f = f ()

[<AutoOpen>]
module IOExpression = 
  let io = IOExpression ()
