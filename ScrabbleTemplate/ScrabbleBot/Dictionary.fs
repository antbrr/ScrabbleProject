module internal Dictionary

type Dictionary = Set<string>

let empty (_:unit) : Dictionary = Set.empty

let insert word dict : Dictionary = Set.add word dict

let lookup (word:string) dict : bool = Set.contains word dict



