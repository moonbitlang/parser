fn _for_in_loop() -> Unit {
  // semicolon, pass
  for _ in [1,2,3]; {}
  let _ = [for x in [1,2,3]; if true => x]
  let _ = [for x in [1,2,3]; => x]
  
  // newline, pass
  for _ in [1,2,3] 
  {}
  let _ = [for x in [1,2,3]
            if true => x]
  let _ = [for x in [1,2,3]
            => x]

  // semicolon + newline, pass 
  for _ in [1,2,3];
  {}
  let _ = [for x in [1,2,3];
            if true => x]
  let _ = [for x in [1,2,3];
            => x]
}

///|
fn _for_in_loop_with_acc() -> Unit {
  // semicolon, pass
  for _ in [1,2,3]; acc = 10; {}
  let _ = [for x in [1,2,3]; acc = 0; if true => x] 
  let _ = [for x in [1,2,3]; acc = 0; => x]

  // newline, pass
  for _ in [1,2,3]; acc = 10
  {}
  let _ = [for x in [1,2,3]; acc = 0
            if true => x] 
  let _ = [for x in [1,2,3]; acc = 0
            => x]

  // semicolon + newline, pass
  for _ in [1,2,3]; acc = 10;
  {}
  let _ = [for x in [1,2,3]; acc = 0;
            if true => x] 
  let _ = [for x in [1,2,3]; acc = 0;
            => x]
}

///|
fn _for_in_loop_with_acc_and_update() -> Unit {
  // semicolon, failed
  for _ in [1,2,3]; acc = 10; acc=acc+1; {}
  let _ = [for x in [1,2,3]; acc = 0; acc=acc+1; if true => x] 
  let _ = [for x in [1,2,3]; acc = 0; acc=acc+1; => x]

  // newline, failed
  for _ in [1,2,3]; acc = 10; acc=acc+1
  {}
  let _ = [for x in [1,2,3]; acc = 0; acc=acc+1
            if true => x] 
  let _ = [for x in [1,2,3]; acc = 0; acc=acc+1
            => x]
  
  // semicolon + newline, failed
  for _ in [1,2,3]; acc = 10; acc=acc+1;
  {}
  let _ = [for x in [1,2,3]; acc = 0; acc=acc+1;
            if true => x] 
  let _ = [for x in [1,2,3]; acc = 0; acc=acc+1;
            => x]
}

///|
fn _for_in_loop_with_double_semi() -> Unit {
  // pass
  for _ in [1,2,3];; {}
  let _ = [for x in [1,2,3];; if true => x] 
  let _ = [for x in [1,2,3];; => x]

  // newline, pass
  for _ in [1,2,3];; 
  {}
  let _ = [for x in [1,2,3];;
            if true => x] 
  let _ = [for x in [1,2,3];;
            => x]
}

///|
fn _for_loop() -> Unit {
  // semicolon, failed
  for x=0;x<10;x=x+1; {}
  let _ = [for x=0;x<10;x=x+1; if e => x]
  let _ = [for x=0;x<10;x=x+1; => x]

  // newline, failed
  for x=0;x<10;x=x+1
  {}
  let _ = [for x=0;x<10;x=x+1
            if e => x]
  let _ = [for x=0;x<10;x=x+1
            => x]

  // semicolon + newline, failed
  for x=0;x<10;x=x+1;
  {}
  let _ = [for x=0;x<10;x=x+1;
            if e => x]
  let _ = [for x=0;x<10;x=x+1;
            => x]
}

///|
fn _for_loop_with_binders_only() -> Unit {
  // semicolon, failed
  // NOTE: it seems there is syntax confliction between `for x=0; { body }` and `for x=0; { condition }`
  for x=0,y=1; {}
  let _ = [for x=0;y=1; if e => x]
  let _ = [for x=0;y=1; => x]

  // newline, failed
  for x=0,y=1
  {}
  let _ = [for x=0,y=1 
            if e => x]
  let _ = [for x=0,y=1
            => x]

  // semicolon + newline, failed
  for x=0,y=1;
  {}
  let _ = [for x=0,y=1;
            if e => x]
  let _ = [for x=0,y=1;
            => x]
}


///|
fn _for_loop_with_binders_and_double_semi() -> Unit {
  // pass
  for x=0;; {}
  let _ = [for x=0;; if e => x]
  let _ = [for x=0;; => x]

  // newline, pass
  for x=0;; 
  {}
  let _ = [for x=0;; 
            if e => x]
  let _ = [for x=0;; 
            => x]
}

///|
fn _for_loop_with_double_semi() -> Unit {
  // pass
  for ;; {}
  let _ = [for ;; if e => x]
  let _ = [for ;; => x]

  // newline, pass
  for ;; 
  {}
  let _ = [for ;; 
            if e => x]
  let _ = [for ;; 
            => x]
}
