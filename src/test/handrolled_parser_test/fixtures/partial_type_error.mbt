struct stock {
    price : int 
    name : string
}

enum lst[T] {
    Nil 
    Cons(cell[T])
}

struct cell[T] {
    data : T
    next : lst [T]
}

fu