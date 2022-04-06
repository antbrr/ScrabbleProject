module internal Trie

type Trie = | Node of bool * Map<char, Trie>

//bool : is it end of word?

//char: The char we want to insert in the trie

//Trie: a new Trie with the next letter that the current is linked to

  
let empty = Node (false, Map.empty)
  
let insert (s:string) dict =
    let rec aux (Node(isword, children)) =
        function
        | [] -> Node (true, children)
        | x::xs ->
            match Map.tryFind x children with
            | None ->
                let child = aux empty xs
                Node(isword, Map.add x child children)
            | Some node ->
                let child = aux node xs
                Node(isword, Map.add x child children)
    aux dict (List.ofSeq s)
     
let lookup (s: string) dict =
    let rec aux (Node(isword, children)) =
        function
        | [] -> isword
        | x::xs ->
            match Map.tryFind x children with
            | None -> false
            | Some node -> aux node xs
    aux dict (List.ofSeq s)
    
    
let step (c: char) (Node (b, children)) : (bool * Trie) option =
    match Map.tryFind c children with
    | None -> None
    | Some (Node(isword, children)) -> Some (isword, Node(isword, children))
    
        
    