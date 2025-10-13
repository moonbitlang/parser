//!build: -w -27
struct T {
    x : Int
} 

type U (Int) -> Int


fn init {
    U::U(x => x)|>ignore
    T::{ x : 2} |> ignore
}