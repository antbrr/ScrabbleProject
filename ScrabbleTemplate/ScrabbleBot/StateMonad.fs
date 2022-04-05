// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun _ -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))

    let wordLength : SM<int> = 
        S (fun s -> Success (s.word.Length, s))

    let characterValue (pos : int) : SM<char> = 
        S (fun s -> 
        match s with
        | s when pos >= s.word.Length || pos < 0 -> Failure (IndexOutOfBounds pos)
        | s -> Success (fst(s.word.[pos]), s))

    let pointValue (pos : int) : SM<int> =
        S (fun s -> 
        match s with
        | s when pos >= s.word.Length || pos < 0 -> Failure (IndexOutOfBounds pos)
        | s -> Success (snd(s.word.[pos]), s))  

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux s.vars with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        S (fun s ->
            match s with
            | s when s.vars = [] -> Failure (VarNotFound "s.vars is empty")
            | s when s.reserved.Contains(var) -> Failure (ReservedName var)
            | s when s.vars.[0].ContainsKey(var) -> Failure (VarExists var)
            | _ -> Success (() ,{s with vars = (s.vars.[0].Add(var, 0)) :: List.tail s.vars}))

    let update (var : string) value : SM<unit> =
        let rec aux (revised: Map<string, 'b> list) notRevised =
            match notRevised with
            | []      -> (None, List.Empty)
            | m :: ms -> 
                match Map.tryFind var m with
                | Some v -> (Some v, revised @ ((Map.add var value m) :: ms))
                | _  -> aux (revised @ [m]) ms

        S (fun s -> 
              match aux List.Empty s.vars with
              | Some _, m -> Success ((), {s with vars = m})
              | None, _   -> Failure (VarNotFound var))    