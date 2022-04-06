// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modules Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x) <?> "whitespace"
    let pletter = satisfy (fun x -> System.Char.IsLetter x)  <?> "letter"
    let pletters1 = many pletter  <?> "letter"
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit x)  <?> "alphanumeric"
    let palphanumerics = many palphanumeric <?> "alphanumerics"
    let pdigit = satisfy (fun x -> System.Char.IsDigit x) <?> "digit"

    let spaces         = many (satisfy (fun x -> System.Char.IsWhiteSpace x)) <?> "space"
    let spaces1        = many1 (satisfy (fun x -> System.Char.IsWhiteSpace x)) <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2

    let parenthesise (p:Parser<'a>) = pchar '(' >*>. p .>*> pchar ')'

    let pid = 
        let aux (l: list<char>) = Seq.ofList l |> System.String.Concat
        pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (a,b) -> aux (a::b)

    let unop op a = op >*>. a

    let binop op p1 p2 = p1 .>*>. unop op p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharacterParse, cref = createParserForwardedToRef<cExp>()
    let BoolParse, bref = createParserForwardedToRef<bExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    

    let VParse = pid |>> V <?> "Var"
    let NParse   = pint32 |>> N <?> "Int"
    let NegParse = (pchar '-' >>. NParse) |>> (fun x -> Mul ((N -1), x)) <?> "Neg"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"


    let AexpParse = TermParse 
    let ParCParse = parenthesise CharacterParse
    let ParBParse = parenthesise BoolParse

    let CharToIntParse = pCharToInt >*>. ParCParse|>> CharToInt <?> "CharToInt"
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let CVParse = pCharValue >*>. ParParse |>> CV <?> "CV"
    let ToUpperParse = pToUpper >*>. ParCParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse = pToLower >*>. ParCParse |>> ToLower <?> "ToLower"
    let IntToCharParse = pIntToChar >*>. ParParse|>> IntToChar <?> "IntToChar"

    let TTParse:Parser<bExp> = pTrue .>*> ParBParse |>> (fun _ -> TT) <?> "TT"
    let FFParse:Parser<bExp> = pFalse .>*> ParBParse |>> (fun _ -> FF) <?> "FF"

    let AEqParse:Parser<bExp> = (AexpParse .>*> (pchar ',') .>*>. AexpParse) .>*> ParBParse |>> AEq <?> "AEq"
    let ALtParse:Parser<bExp> = (AexpParse .>*> (pchar ',') .>*>. AexpParse) .>*> ParBParse |>> ALt <?> "ALt"
    let NotParse:Parser<bExp> = ParBParse |>> Not <?> "Not"
    let Conj:Parser<bExp> = (BoolParse .>*> (pchar ',') .>*>. BoolParse) .>*> ParBParse |>> Conj <?> "Conj"
    let IsVowelParse:Parser<bExp> = pIsVowel >*>.ParCParse |>> IsVowel <?> "IsVowel"
    let IsDigitParse:Parser<bExp> = pIsDigit >*>. ParCParse |>> IsDigit <?> "IsDigit"
    let IsLetterParse:Parser<bExp> = pIsLetter >*>.ParCParse |>> IsLetter <?> "IsLetter"

    do tref := choice [AddParse; SubParse; ProdParse;]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [CharToIntParse; PVParse; NegParse; VParse; NParse; ParParse]
    do cref := choice [CVParse; IntToCharParse; ToUpperParse; ToLowerParse; CParse; ParCParse]
    do bref := choice [ParBParse]

    let CexpParse = CharacterParse

    let BexpParse = BoolParse
    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
