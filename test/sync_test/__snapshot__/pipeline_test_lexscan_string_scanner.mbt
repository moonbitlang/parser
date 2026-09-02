fn scan(scanner : @lexbuf.StringScanner) -> String {
  lexscan scanner {
    (re"^[a-z]+") as word => "word:\{word}"
    re"^[0-9]+" => "int"
    re"^ +" => scan(scanner)
    _ => "other"
  }
}

fn main {
  let scanner = @lexbuf.StringScanner::{ data: "abc 123 ?!"[:], cursor: 0 }
  println(scan(scanner))
  println(scan(scanner))
  println(scan(scanner))
  println(scan(scanner))
  println(scanner.cursor)
}
