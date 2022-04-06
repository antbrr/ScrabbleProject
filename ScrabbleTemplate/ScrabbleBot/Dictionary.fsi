module internal Dictionary

type Dictionary = Set<string>
    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool

