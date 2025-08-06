fn f(cb : (int)-> string, r : int){
    cb(r)
  }

fn f (cb : ((list[int])->string), r : int) {cb(r)}


let a = {1;2}

let x = {let a = 1 ; 2} 


let a = Cons(1,Cons(2,Cons(3)))
let b = Cons(1,Cons(2,Cons(3,a)))

fn hello(lst){
    match lst {
        Nil => 0 
        Cons((_,Cons((_,rest)))) => 2 + hello(rest)
        Cons((_,rest)) => 1 + hello(rest)
    }
   }

fn[A,B] map (xs : list[A], f : (A) -> B) -> list[B] {
        match xs {
            Nil => Nil
            Cons((x, xs)) => Cons(f(x), map(f,xs))
        }   


           let x = fn (x,y) {1}    
           fn even (x) {
             if x == 0 {
               true
             } else {
               odd (x - 1)
             }
           }
           fn odd(x) {
             if x == 1 {
               true
             } else {
               even ( x - 1)
             }
           }
           let z = 1
           struct u{
             x : int
             y : v
           }
           struct v{
             x : int
             h : u
           }


enum a{
  A(int)
  B
  }
let x = A
