namespace TimeKeeper

type IO<'a> = IO of (unit -> 'a)

module IO = 
  let run (IO f) = f ()
  let make f a = IO (fun () -> f a)
  let make2 f a b = make (f a) b
  let make3 f a b c = make (f a b) c
  let make4 f a b c d = make (f a b c) d
  let make5 f a b c d e = make (f a b c d) e

  let bind expr f = 
    let evaluate () =
      run f
      |> expr
      |> run

    IO evaluate

  let map expr f =
    let ret x = IO (fun () -> x) 

    f |> bind (expr >> ret)


type IOExpression() =
  member this.Bind (f, cexpr) = IO.bind cexpr f    

  member this.Return x = IO (fun () -> x)

  member this.ReturnFrom x = x

  member this.Zero () = this.Return ()

[<AutoOpen>]
module IOExpression = 
  let io = IOExpression ()
