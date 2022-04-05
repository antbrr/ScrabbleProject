// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add (a:SM<int>) (b:SM<int>) = 
        a >>= fun x -> 
        b >>= fun y -> 
        ret (x + y)

    let sub a b =
        a >>= fun x ->
        b >>= fun y ->
        ret (x - y)

    let div a b =
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (x / y) else fail DivisionByZero
    
    let mul a b =
      a >>= fun x ->
      b >>= fun y ->
      ret (x * y)
    
    let modulo a b =
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (x % y) else fail DivisionByZero

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval (a: aExp) : SM<int> = 
        match a with
        | N x -> ret (x)
        | V s -> lookup s 
        | WL -> wordLength
        | PV pos ->  arithEval pos >>= pointValue
        | Add (a,b) -> add (arithEval a) (arithEval b)
        | Sub (a,b) -> sub (arithEval a) (arithEval b)
        | Mul (a,b) -> mul (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt c -> (charEval c) >>= fun x -> ret (int x)

    and charEval c : SM<char> =
        match c with
        | C c -> ret (c) (* Character value *)
        | CV pos -> arithEval pos >>= characterValue (* Character lookup at word index *)
        | ToUpper c -> (charEval c) >>= fun c' -> charEval (C (System.Char.ToUpper c'))
        | ToLower c -> (charEval c) >>= fun c' -> charEval (C (System.Char.ToLower c'))
        | IntToChar a -> (arithEval a) >>= fun x -> ret (char x)

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret (true)
        | FF -> ret (false)

        | AEq (a,b) -> (arithEval a) >>= fun x -> (arithEval b) >>= fun y -> ret (x = y)
        | ALt (a,b) -> (arithEval a) >>= fun x -> (arithEval b) >>= fun y -> ret (x < y)

        | Not b -> (boolEval b) >>= fun b' -> ret (not b')
        | Conj (a,b) -> (boolEval a) >>= fun x -> (boolEval b) >>= fun y -> ret (x && y)

        | IsVowel c -> (charEval c) >>= fun c' -> ret ("AEIOUYaeiouy".Contains(c'))
        | IsLetter c -> (charEval c) >>= fun c' -> ret (System.Char.IsLetter c')
        | IsDigit c -> (charEval c) >>= fun c' -> ret (System.Char.IsDigit c')


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare str -> declare str
        | Ass (str, a) -> (arithEval a) >>= fun a' -> update str a'
        | Skip -> (stmntEval stmnt)
        | Seq (stm1, stm2) -> (stmntEval stm1) >>>= (stmntEval stm2)
        | ITE (guard, stm1, stm2) -> push >>>= (boolEval guard) >>= fun guard'-> if guard' then (stmntEval stm1) else (stmntEval stm2)
        | While (guard, stm) -> push >>>= (boolEval guard) >>= fun guard' -> if guard' then (stmntEval (While (guard, stm))) else (stmntEval stm)


(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

   let rec arithEval2 a = 
        match a with
        | N x -> ret x
        | V s -> lookup s 
        | WL -> wordLength
        | PV pos ->  prog{
            let! pos' = (arithEval2 pos)
            return! pointValue pos'
            }
        | Add (a,b) -> add (arithEval2 a) (arithEval2 b)
        | Sub (a,b) -> sub (arithEval2 a) (arithEval2 b)
        | Mul (a,b) -> mul (arithEval2 a) (arithEval2 b)
        | Div (a,b) -> div (arithEval2 a) (arithEval2 b)
        | Mod (a,b) -> modulo (arithEval2 a) (arithEval2 b)
        | CharToInt c -> prog{
            let! c' = (charEval2 c)
            return int c'
        }
    

    and charEval2 c =
        match c with
        | C c -> ret (c)
        | CV pos -> prog{
            let! pos' = (arithEval2 pos)
            return! characterValue pos'
            }
        | ToUpper c -> prog{
                let! c' = (charEval2 c)
                return! charEval2 (C (System.Char.ToUpper c'))
            }
        | ToLower c -> prog{
                let! c' = (charEval2 c)
                return! charEval2 (C (System.Char.ToLower c'))
            }
        | IntToChar a -> prog{
            let! a' = (arithEval2 a)
            return char a'
        }

    let rec boolEval2 b = 
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq (a,b) -> prog{
            let! a' = (arithEval2 a)
            let! b' = (arithEval2 b)
            return a' = b'
            }
        | ALt (a,b) -> prog{
            let! a' = (arithEval2 a)
            let! b' = (arithEval2 b)
            return a' < b'
            }

        | Not b -> prog{
            let! b' = (boolEval2 b)
            return not b'
            }

        | Conj (a,b) -> prog{
            let! a' = (boolEval2 a)
            let! b' = (boolEval2 b)
            return a' && b'
            }

        | IsVowel c -> prog{
            let! c' = (charEval2 c)
            return "AEIOUYaeiouy".Contains(c')
            }
        | IsLetter c -> prog{
            let! c' = (charEval2 c)
            return System.Char.IsLetter c'
            }
        | IsDigit c -> prog{
            let! c' = (charEval2 c)
            return System.Char.IsDigit c'
            }

    let rec stmntEval2 stmnt =
        match stmnt with
        | Declare str -> declare str
        | Ass (str, a) -> prog{
            let! a' = arithEval2 a
            return! update str a'
            }
        | Skip -> (stmntEval2 stmnt)
        | Seq (stm1, stm2) -> prog{
            do! (stmntEval2 stm1)
            do! (stmntEval2 stm2)
            }
        | ITE (guard, stm1, stm2) -> prog{
            do! push
            let! guard' = (boolEval2 guard)
            return! if guard' then (stmntEval2 stm1) else (stmntEval2 stm2)
            }
        | While (guard, stm) -> prog{
            let! guard' = (boolEval2 guard)
            return! if guard' then (stmntEval2 (While (guard, stm))) else (stmntEval2 stm)
            }

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stmnt = //does not work!!!
        (fun (word:word) (pos:int) acc ->
            (stmntEval2 stmnt) |> evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] word ["_pos_"; "_acc_";"_result_"])
        )


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"