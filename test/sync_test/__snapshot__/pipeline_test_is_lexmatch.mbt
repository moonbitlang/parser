//!build: -w -20-27-unused_lexcase

fn init {
  // Basic exact match tests
  println(lexmatch "abc" { re"^abc$" => true; _ => false })
  println(lexmatch "abcd" { re"^abc$" => true; _ => false })
  println(lexmatch "abc" { (re"^abc", after=x) => x == ""; _ => false })
  println(lexmatch "abcd" { (re"^abc", after=x) => x == "d"; _ => false })
  
  // Longest match tests
  println(lexmatch "abc" with longest { re"^abc$" => true; _ => false })
  println(lexmatch "abcd" with longest { re"^abc$" => true; _ => false })
  println(lexmatch "abc" with longest { (re"^abc", after=x) => x == ""; _ => false })
  println(lexmatch "abcd" with longest { (re"^abc", after=x) => x == "d"; _ => false })
  
  // Empty string tests
  println(lexmatch "" { re"^$" => true; _ => false })
  println(lexmatch "" { (re"^", after=x) => x == ""; _ => false })
  println(lexmatch "abc" { re"^$" => true; _ => false })
  println(lexmatch "" { re"^abc$" => true; _ => false })
  
  // Partial match tests
  println(lexmatch "xyz" { re"^abc$" => true; _ => false })
  println(lexmatch "axyz" { re"^a$" => true; _ => false })
  println(lexmatch "axyz" { (re"^a", after=rest) => rest == "xyz"; _ => false })
  
  // Multiple character matches
  println(lexmatch "hello world" { re"^hello$" => true; _ => false })
  println(lexmatch "hello world" { (re"^hello", after=x) => x == " world"; _ => false })
  println(lexmatch "hello" { re"^hello world$" => true; _ => false })
  
  // Special characters and patterns
  println(lexmatch "123abc" { re"^123$" => true; _ => false })
  println(lexmatch "123abc" { (re"^123", after=x) => x == "abc"; _ => false })
  println(lexmatch "!@#$%" { re"^!@#$" => true; _ => false })
  println(lexmatch "!@#$%" { (re"^!@#", after=x) => x == "$%"; _ => false })
  
  // Unicode and international characters
  println(lexmatch "你好世界" { re"^你好$" => true; _ => false })
  println(lexmatch "你好世界" { (re"^你好", after=x) => x == "世界"; _ => false })
  println(lexmatch "café" { re"^caf$" => true; _ => false })
  println(lexmatch "café" { (re"^caf", after=x) => x == "é"; _ => false })
  
  // Longer strings
  println(lexmatch "abcdefghij" { re"^abcde$" => true; _ => false })
  println(lexmatch "abcdefghij" { (re"^abcde", after=x) => x == "fghij"; _ => false })
  
  // Single character tests
  println(lexmatch "a" { re"^a$" => true; _ => false })
  println(lexmatch "a" { (re"^a", after=x) => x == ""; _ => false })
  println(lexmatch "ab" { (re"^a", after=x) => x == "b"; _ => false })
  
  // Whitespace tests
  println(lexmatch " abc" { re"^ $" => true; _ => false })
  println(lexmatch " abc" { (re"^ ", after=x) => x == "abc"; _ => false })
  println(lexmatch "abc " { re"^abc$" => true; _ => false })
  println(lexmatch "abc " { (re"^abc", after=x) => x == " "; _ => false })
  println(lexmatch "\tabc" { re"^\t$" => true; _ => false })
  println(lexmatch "\nabc" { re"^\n$" => true; _ => false })
  
  // Numbers and digits
  println(lexmatch "0123456789" { re"^012345$" => true; _ => false })
  println(lexmatch "0123456789" { (re"^012345", after=x) => x == "6789"; _ => false })
  
  // Mixed alphanumeric
  println(lexmatch "abc123def456" { re"^abc123$" => true; _ => false })
  println(lexmatch "abc123def456" { (re"^abc123", after=x) => x == "def456"; _ => false })
  
  // Case sensitivity
  println(lexmatch "ABC" { re"^ABC$" => true; _ => false })
  println(lexmatch "ABC" { re"^abc$" => true; _ => false })
  println(lexmatch "abc" { re"^ABC$" => true; _ => false })
  
  // Repeated characters
  println(lexmatch "aaa" { re"^aa$" => true; _ => false })
  println(lexmatch "aaa" { (re"^aa", after=x) => x == "a"; _ => false })
  println(lexmatch "aaabbb" { re"^aaa$" => true; _ => false })
  println(lexmatch "aaabbb" { (re"^aaa", after=x) => x == "bbb"; _ => false })
  
  // Longest match with multiple options
  println(lexmatch "test" with longest { re"^test$" => true; _ => false })
  println(lexmatch "testing" with longest { re"^test$" => true; _ => false })
  println(lexmatch "testing" with longest { (re"^test", after=x) => x == "ing"; _ => false })
  
  // Edge cases with rest being empty vs non-empty
  println(lexmatch "match" { (re"^match", after=x) => x == ""; _ => false })
  println(lexmatch "matched" { (re"^match", after=x) => x == "ed"; _ => false })
  println(lexmatch "m" { (re"^m", after=x) => x == ""; _ => false })

  // Case insensitive
  println(lexmatch "Abc" { re"^(?i:ABC)$" => true; _ => false })
  println(lexmatch "AbCdEf" { re"^(?i:[a-f]+)$" => true; _ => false })
  println(lexmatch "Abc" with longest { re"^(?i:ABC)$" => true; _ => false })
  println(lexmatch "AbCdEf" with longest { re"^(?i:[a-f]+)$" => true; _ => false })

  let rest = lexmatch "hello1234abc" {
    (re"^hello[0-9]+", after=rest) => Some(rest)
    _ => None
  }
  if rest is Some(rest) {
    println(rest)
  }
}
