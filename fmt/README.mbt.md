# moonbitlang/formatter 

`moonbitlang/formatter` is a code formatter for the MoonBit. This project was 
created with the following goals in mind:

- Ensure readability and consistent code formatting.
- Provide a single "true" formatting style, with no configuration required, making it easy to adopt across teams.
- Guarantee idempotence: formatting the code multiple times yields the same result after the first application.
- Properly handle comments and merge blank lines.

**Warning: This module is highly experimental and work in progress.**

## Status

* [x] Parenthesis inference
* [x] Expression layout composition
* [x] Expression formatting
* [x] Top-level declaration formatting
* [ ] Comment formatting
* [ ] Blank line awareness
* [ ] Fully test coverage

## Usage

```mbt nocheck
///|
test {
  let code =
    #|/// Tree data structure
    #|pub enum Tree[T] { Empty; Node(Tree[T], T, Tree[T]) }derive(Show)
    #|
    #|/// Greet a person
    #|pub fn hello(name:String)->Unit{ let str = "hello, \{name}!"; println(str) } 
  let result = @formatter.format(code)
  println(result)
  inspect(
    result,
    content=(
      #|///|
      #|/// Tree data structure
      #|pub enum Tree[T] {
      #|  Empty
      #|  Node(Tree[T], T, Tree[T])
      #|} derive(Show)
      #|
      #|///|
      #|/// Greet a person
      #|pub fn hello(name : String) -> Unit {
      #|  let str = "hello, name!"
      #|  println(str)
      #|}
      #|
      #|
    ),
  )
}
```
## Contributing

At this early stage, we are not ready to accept pull requests yet. 
You can contribute by using this module in your projects and providing feedback. 
If you encounter any bugs or poor formatting results, please open an issue.
