//!build: -w -20-27

fn init {
  // Basic exact match tests
  println(lexscan "abc" { re"^abc$" => true; _ => false })
  println(lexscan "abcd" { re"^abc$" => true; _ => false })
  println(lexscan "abc" { (re"^abc", after=x) => x == ""; _ => false })
  println(lexscan "abcd" { (re"^abc", after=x) => x == "d"; _ => false })
  
  // Longest match tests
  println(lexscan "abc" with longest { re"^abc$" => true; _ => false })
  println(lexscan "abcd" with longest { re"^abc$" => true; _ => false })
  println(lexscan "abc" with longest { (re"^abc", after=x) => x == ""; _ => false })
  println(lexscan "abcd" with longest { (re"^abc", after=x) => x == "d"; _ => false })
  
  // Empty string tests
  println(lexscan "" { re"^$" => true; _ => false })
  println(lexscan "" { (re"^", after=x) => x == ""; _ => false })
  println(lexscan "abc" { re"^$" => true; _ => false })
  println(lexscan "" { re"^abc$" => true; _ => false })
  
  // Partial match tests
  println(lexscan "xyz" { re"^abc$" => true; _ => false })
  println(lexscan "axyz" { re"^a$" => true; _ => false })
  println(lexscan "axyz" { (re"^a", after=rest) => rest == "xyz"; _ => false })
  
  // Multiple character matches
  println(lexscan "hello world" { re"^hello$" => true; _ => false })
  println(lexscan "hello world" { (re"^hello", after=x) => x == " world"; _ => false })
  println(lexscan "hello" { re"^hello world$" => true; _ => false })
  
  // Special characters and patterns
  println(lexscan "123abc" { re"^123$" => true; _ => false })
  println(lexscan "123abc" { (re"^123", after=x) => x == "abc"; _ => false })
  println(lexscan "!@#$%" { re"^!@#$" => true; _ => false })
  println(lexscan "!@#$%" { (re"^!@#", after=x) => x == "$%"; _ => false })
  
  // Unicode and international characters
  println(lexscan "你好世界" { re"^你好$" => true; _ => false })
  println(lexscan "你好世界" { (re"^你好", after=x) => x == "世界"; _ => false })
  println(lexscan "café" { re"^caf$" => true; _ => false })
  println(lexscan "café" { (re"^caf", after=x) => x == "é"; _ => false })
  
  // Longer strings
  println(lexscan "abcdefghij" { re"^abcde$" => true; _ => false })
  println(lexscan "abcdefghij" { (re"^abcde", after=x) => x == "fghij"; _ => false })
  
  // Single character tests
  println(lexscan "a" { re"^a$" => true; _ => false })
  println(lexscan "a" { (re"^a", after=x) => x == ""; _ => false })
  println(lexscan "ab" { (re"^a", after=x) => x == "b"; _ => false })
  
  // Whitespace tests
  println(lexscan " abc" { re"^ $" => true; _ => false })
  println(lexscan " abc" { (re"^ ", after=x) => x == "abc"; _ => false })
  println(lexscan "abc " { re"^abc$" => true; _ => false })
  println(lexscan "abc " { (re"^abc", after=x) => x == " "; _ => false })
  println(lexscan "\tabc" { re"^\t$" => true; _ => false })
  println(lexscan "\nabc" { re"^\n$" => true; _ => false })
  
  // Numbers and digits
  println(lexscan "0123456789" { re"^012345$" => true; _ => false })
  println(lexscan "0123456789" { (re"^012345", after=x) => x == "6789"; _ => false })
  
  // Mixed alphanumeric
  println(lexscan "abc123def456" { re"^abc123$" => true; _ => false })
  println(lexscan "abc123def456" { (re"^abc123", after=x) => x == "def456"; _ => false })
  
  // Case sensitivity
  println(lexscan "ABC" { re"^ABC$" => true; _ => false })
  println(lexscan "ABC" { re"^abc$" => true; _ => false })
  println(lexscan "abc" { re"^ABC$" => true; _ => false })
  
  // Repeated characters
  println(lexscan "aaa" { re"^aa$" => true; _ => false })
  println(lexscan "aaa" { (re"^aa", after=x) => x == "a"; _ => false })
  println(lexscan "aaabbb" { re"^aaa$" => true; _ => false })
  println(lexscan "aaabbb" { (re"^aaa", after=x) => x == "bbb"; _ => false })
  
  // Longest match with multiple options
  println(lexscan "test" with longest { re"^test$" => true; _ => false })
  println(lexscan "testing" with longest { re"^test$" => true; _ => false })
  println(lexscan "testing" with longest { (re"^test", after=x) => x == "ing"; _ => false })
  
  // Edge cases with rest being empty vs non-empty
  println(lexscan "match" { (re"^match", after=x) => x == ""; _ => false })
  println(lexscan "matched" { (re"^match", after=x) => x == "ed"; _ => false })
  println(lexscan "m" { (re"^m", after=x) => x == ""; _ => false })

  // Case insensitive
  println(lexscan "Abc" { re"^(?i:ABC)$" => true; _ => false })
  println(lexscan "AbCdEf" { re"^(?i:[a-f]+)$" => true; _ => false })
  println(lexscan "Abc" with longest { re"^(?i:ABC)$" => true; _ => false })
  println(lexscan "AbCdEf" with longest { re"^(?i:[a-f]+)$" => true; _ => false })

  let rest = lexscan "hello1234abc" {
    (re"^hello[0-9]+", after=rest) => Some(rest)
    _ => None
  }
  if rest is Some(rest) {
    println(rest)
  }
}
