# Overview of formatter design


# Compose expressions

# Reduce the indentation, newline and braces

## Rule 1: Simple Expressions/Patterns Sequence

## Rule 2: Trailing Block
 
## Rule 3: Collapsible Expression in RHS

To reduce the indentation level and improve readbility, we can collapse some expressions in the right hand side of certain constructs. For example:

```moonbit
// good: space is enough after `let short_pattern =`
let short_pattern = oneline_func(arg) 
// bad: extra braces, newline and indentation
let short_pattern = {
  oneline_func(arg)
}
// bad: extra indentation and newline
let short_pattern = 
  oneline_func(arg)
```

```moonbit
// good: space is not enough after `long_pattern =>`,
// move to next line with indentation without extra braces
match e {
  long_pattern =>   
    oneline_func(arg) 
}
// bad
match e {
  long_pattern => oneline_func(
      arg
    ) 
}
// bad: extra braces, newline and indentation
match e {
  long_pattern => {
    oneline_func(arg) 
  }
}
```

```moonbit
// good: space is not enough for the record, but the head `TypeName::{` fits after `long_pattern =>`.
// move the other parts to next line **without** extra indentation. 
match e {
  long_pattern => TypeName::{
    field1: value1,  
    field2: value2,
  }
}
// bad
match e {
  long_pattern => TypeName::{
      field1: value1,  
      field2: value2,
    }
}
// bad
match e {
  long_pattern => {
    TypeName::{
      field1: value1,  
      field2: value2,
    }
  }
}
// bad
match e {
  long_pattern => 
    TypeName::{
      field1: value1,  
      field2: value2,
    }
}
```

This rule applies to the collapsible expression in the right hand side of following constructs:

- `pattern => rhs`
- `let a = rhs`, `let mut a = rhs`
- `binder = rhs`, `arr[i] = rhs`, `x.field = rhs`

The collapsible expression are divided into two parts, `head` and `body`. For example:

```
  x =>   ...
  fn(x) {... }
  fcall (args)
  \---/  \--/
  head   body
  
  match cond { ... }
  if cond    { ... } else { ... }
  \----------/\-----------------/
     head          body

  [ elem1, elem2 ]
  ( elem1, elem2 )
  ( expr : Annotation )
  { field1: value1, field2, value2 }
  \-------------------------------/
      body (head is empty)
```

Consider the acc is the some thing like `pattern => ` or `let a =`:

If the `rhs` fits in the current line, print in flatten style.

```
   +-----+--------+--------+
   | acc |  head  |  body  | 
   +-----+--------+--------+
        flatten style.
```


If the `rhs` fits in the next line with indentation:

```
 +-----------+
 |    acc    |
 +--+--------+--------+
    |  head  |  body  |
    +--------+--------+
  dangling style. 
```

If the head fits in current line, the `rhs` can be composed:

```
 +-----+--------+
 | acc |  head  |
 +-----+--------+          
 |    body      |     
 +--------------+
  composed style.
```

If the head can not fits in the current line, print in normal:
 
```
   +-----------+--+
   |  acc      | {|
   +--+--------+--+-----+
      |  head  |  body  |
   +--+--------+--------+
   |} |
   +--+
      normal style.
```


 

 
