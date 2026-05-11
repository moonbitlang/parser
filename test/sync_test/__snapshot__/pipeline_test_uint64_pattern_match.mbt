

fn pop_count(c : UInt64) -> Int { 
   for count = 0, word = c {
     match (count, word) {
       (count, 0) => break count
       (count, word) => continue count + 1, word & (word - 1)
     }
   }
 } 


fn main {
  // test pop_count
  println("===== testing pop_count:")
  println(pop_count(64))
 }
