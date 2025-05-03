
// for string
let string_repr_buf : StringBuilder = StringBuilder::new()
let string_interps : Ref[Array[InterpElem]] = @ref.new([])

fn string(lexbuf : Lexbuf, env~ : LexEnv, end_with_newline~ : Bool, allow_interp~ : Bool, startpos~ : Int) -> Array[InterpElem] {
  string_repr_buf.reset()
  normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
  if string_interps.val.length() == 0 {
    [
      InterpLit(
        repr = "",
        loc = Location::{ start : env.make_pos(startpos), end : env.make_pos(lexbuf.curr_pos()) }
      )
    ]
  } else {
    let interps = string_interps.val
    string_interps.val = []
    return interps
  }
}

// invalid byte

let invalid_byte_repr_buf : StringBuilder = StringBuilder::new()



fn interp_handle(lexbuf : Lexbuf, env~ : LexEnv) -> Int  {
  // The matched pattern id
  let mut matched = @int.max_value
  let mut _end_pos = -1
  let mut _start_pos = lexbuf.curr_pos()
  let mut _capture_0_start = -1
  let mut _capture_0_end = -1
  let mut _capture_1_start = -1
  let mut _capture_1_end = -1
  loop 0 {
    0 => {
      continue match lexbuf.next_as_int() {
        -1 => 1
        0..=8 => 2
        9 => 3
        10 => 4
        11..=12 => 3
        13 => 4
        14..=31 => 2
        32 => 3
        33 => 2
        34 => 5
        35..=122 => 2
        123 => 5
        124 => 2
        125 => 6
        126..=127 => 2
        194 => 7
        195..=223 => 8
        224 => 9
        225 => 10
        226 => 11
        227 => 12
        228..=236 => 9
        237 => 13
        238 => 9
        239 => 14
        240 => 15
        241..=244 => 16
        _ => break
      }
    }
    1 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    2 => {
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    3 => {
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        9 => 22
        11..=12 => 22
        32 => 22
        125 => 6
        194 => 23
        225 => 24
        226 => 25
        227 => 26
        239 => 27
        _ => break
      }
    }
    4 => {
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    5 => {
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    6 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    7 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 2
        160 => 3
        161..=191 => 2
        _ => break
      }
    }
    8 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 2
        _ => break
      }
    }
    9 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 8
        _ => break
      }
    }
    10 => {
      continue match lexbuf.next_as_int() {
        128..=153 => 8
        154 => 19
        155..=191 => 8
        _ => break
      }
    }
    11 => {
      continue match lexbuf.next_as_int() {
        128 => 20
        129 => 21
        130..=191 => 8
        _ => break
      }
    }
    12 => {
      continue match lexbuf.next_as_int() {
        128 => 19
        129..=191 => 8
        _ => break
      }
    }
    13 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 8
        160..=191 => 17
        _ => break
      }
    }
    14 => {
      continue match lexbuf.next_as_int() {
        128..=186 => 8
        187 => 18
        188..=191 => 8
        _ => break
      }
    }
    15 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 9
        _ => break
      }
    }
    16 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 9
        _ => break
      }
    }
    17 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 5
        _ => break
      }
    }
    18 => {
      continue match lexbuf.next_as_int() {
        128..=190 => 2
        191 => 3
        _ => break
      }
    }
    19 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        129..=191 => 2
        _ => break
      }
    }
    20 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 3
        139..=174 => 2
        175 => 3
        176..=191 => 2
        _ => break
      }
    }
    21 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 2
        159 => 3
        160..=191 => 2
        _ => break
      }
    }
    22 => {
      continue match lexbuf.next_as_int() {
        9 => 22
        11..=12 => 22
        32 => 22
        125 => 6
        194 => 23
        225 => 24
        226 => 25
        227 => 26
        239 => 27
        _ => break
      }
    }
    23 => {
      continue match lexbuf.next_as_int() {
        160 => 22
        _ => break
      }
    }
    24 => {
      continue match lexbuf.next_as_int() {
        154 => 29
        _ => break
      }
    }
    25 => {
      continue match lexbuf.next_as_int() {
        128 => 30
        129 => 31
        _ => break
      }
    }
    26 => {
      continue match lexbuf.next_as_int() {
        128 => 29
        _ => break
      }
    }
    27 => {
      continue match lexbuf.next_as_int() {
        187 => 28
        _ => break
      }
    }
    28 => {
      continue match lexbuf.next_as_int() {
        191 => 22
        _ => break
      }
    }
    29 => {
      continue match lexbuf.next_as_int() {
        128 => 22
        _ => break
      }
    }
    30 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 22
        175 => 22
        _ => break
      }
    }
    31 => {
      continue match lexbuf.next_as_int() {
        159 => 22
        _ => break
      }
    }
    _ => panic()
  }

  guard matched <= 5 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_end_pos)
  match matched {
    0 => {
      ()

      _start_pos
    
    }
    1 => {
      ()

      env.add_lexing_error(start=_start_pos, end = _end_pos, UnterminatedString)
      _start_pos
    
    }
    2 => {
      ()

      env.add_lexing_error(start=_start_pos, end = _end_pos, UnterminatedStringInVariableInterploation)
      lexbuf.reset(pos=_start_pos)
      _start_pos
    
    }
    3 => {
      ()

      env.add_lexing_error(start=_start_pos, end = _end_pos, UnterminatedStringInVariableInterploation)
      _start_pos
    
    }
    4 => {
      ()
      let c = lexbuf.get_char(_capture_0_start, _capture_0_end)

      string_repr_buf.write_char(c)
      interp_handle(lexbuf, env~)
    
    }
    5 => {
      ()
      let c = lexbuf.get_char(_capture_0_start, _capture_0_end)

      // TODO: better error message when it is '"' etc
      env.add_lexing_error(start=_start_pos, end = _end_pos, IllegalCharacter(c))
      interp_handle(lexbuf, env~)
    
    }
    _ => panic()
  }
}


fn normal(lexbuf : Lexbuf, env~ : LexEnv, end_with_newline~ : Bool, allow_interp~ : Bool, startpos~ : Int) -> Unit  {
  // The matched pattern id
  let mut matched = @int.max_value
  let mut _end_pos = -1
  let mut _start_pos = lexbuf.curr_pos()
  let mut _capture_0_start = -1
  let mut _capture_0_end = -1
  let mut _capture_1_start = -1
  let mut _capture_1_end = -1
  loop 0 {
    0 => {
      continue match lexbuf.next_as_int() {
        -1 => 1
        0..=12 => 2
        13 => 3
        14..=33 => 2
        34 => 4
        35..=91 => 2
        92 => 5
        93..=127 => 2
        194..=223 => 6
        224..=239 => 7
        240 => 8
        241..=244 => 9
        _ => break
      }
    }
    1 => {
      if matched >= 11 {
        matched = 11
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    2 => {
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    3 => {
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        10 => 10
        _ => break
      }
    }
    4 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    5 => {
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=31 => 11
        32 => 12
        33 => 11
        34 => 12
        35..=38 => 11
        39 => 12
        40..=91 => 11
        92 => 12
        93..=97 => 11
        98 => 12
        99..=109 => 11
        110 => 12
        111 => 13
        112..=113 => 11
        114 => 12
        115 => 11
        116 => 12
        117 => 14
        118..=119 => 11
        120 => 15
        121..=122 => 11
        123 => 16
        124..=127 => 11
        194..=223 => 17
        224..=239 => 18
        240 => 19
        241..=244 => 20
        _ => break
      }
    }
    6 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 2
        _ => break
      }
    }
    7 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 6
        _ => break
      }
    }
    8 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 7
        _ => break
      }
    }
    9 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 7
        _ => break
      }
    }
    10 => {
      if matched >= 12 {
        matched = 12
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    11 => {
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    12 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 2
      }
      break
    }
    13 => {
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=47 => 28
        48..=51 => 29
        52..=127 => 28
        194..=223 => 30
        224..=239 => 31
        240 => 32
        241..=244 => 33
        _ => break
      }
    }
    14 => {
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 26
        65..=70 => 26
        97..=102 => 26
        123 => 27
        _ => break
      }
    }
    15 => {
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=47 => 34
        48..=57 => 35
        58..=64 => 34
        65..=70 => 35
        71..=96 => 34
        97..=102 => 35
        103..=127 => 34
        194..=223 => 36
        224..=239 => 37
        240 => 38
        241..=244 => 39
        _ => break
      }
    }
    16 => {
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        9 => 16
        11..=12 => 16
        32 => 16
        194 => 21
        225 => 22
        226 => 23
        227 => 24
        239 => 25
        _ => break
      }
    }
    17 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 11
        _ => break
      }
    }
    18 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 17
        _ => break
      }
    }
    19 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 18
        _ => break
      }
    }
    20 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 18
        _ => break
      }
    }
    21 => {
      continue match lexbuf.next_as_int() {
        160 => 16
        _ => break
      }
    }
    22 => {
      continue match lexbuf.next_as_int() {
        154 => 41
        _ => break
      }
    }
    23 => {
      continue match lexbuf.next_as_int() {
        128 => 42
        129 => 43
        _ => break
      }
    }
    24 => {
      continue match lexbuf.next_as_int() {
        128 => 41
        _ => break
      }
    }
    25 => {
      continue match lexbuf.next_as_int() {
        187 => 40
        _ => break
      }
    }
    26 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 51
        65..=70 => 51
        97..=102 => 51
        _ => break
      }
    }
    27 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 44
        11..=12 => 44
        14..=47 => 44
        48..=57 => 45
        58..=64 => 44
        65..=70 => 45
        71..=96 => 44
        97..=102 => 45
        103..=124 => 44
        125 => 46
        126..=127 => 44
        194..=223 => 47
        224..=239 => 48
        240 => 49
        241..=244 => 50
        _ => break
      }
    }
    28 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 52
        194..=223 => 53
        224..=239 => 54
        240 => 55
        241..=244 => 56
        _ => break
      }
    }
    29 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 52
        48..=55 => 57
        56..=127 => 52
        194..=223 => 53
        224..=239 => 54
        240 => 55
        241..=244 => 56
        _ => break
      }
    }
    30 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 28
        _ => break
      }
    }
    31 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 30
        _ => break
      }
    }
    32 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 31
        _ => break
      }
    }
    33 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 31
        _ => break
      }
    }
    34 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 58
        194..=223 => 59
        224..=239 => 60
        240 => 61
        241..=244 => 62
        _ => break
      }
    }
    35 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 58
        48..=57 => 63
        58..=64 => 58
        65..=70 => 63
        71..=96 => 58
        97..=102 => 63
        103..=127 => 58
        194..=223 => 59
        224..=239 => 60
        240 => 61
        241..=244 => 62
        _ => break
      }
    }
    36 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 34
        _ => break
      }
    }
    37 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 36
        _ => break
      }
    }
    38 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 37
        _ => break
      }
    }
    39 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 37
        _ => break
      }
    }
    40 => {
      continue match lexbuf.next_as_int() {
        191 => 16
        _ => break
      }
    }
    41 => {
      continue match lexbuf.next_as_int() {
        128 => 16
        _ => break
      }
    }
    42 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 16
        175 => 16
        _ => break
      }
    }
    43 => {
      continue match lexbuf.next_as_int() {
        159 => 16
        _ => break
      }
    }
    44 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 44
        11..=12 => 44
        14..=124 => 44
        125 => 46
        126..=127 => 44
        194..=223 => 47
        224..=239 => 48
        240 => 49
        241..=244 => 50
        _ => break
      }
    }
    45 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 44
        11..=12 => 44
        14..=47 => 44
        48..=57 => 45
        58..=64 => 44
        65..=70 => 45
        71..=96 => 44
        97..=102 => 45
        103..=124 => 44
        125 => 64
        126..=127 => 44
        194..=223 => 47
        224..=239 => 48
        240 => 49
        241..=244 => 50
        _ => break
      }
    }
    46 => {
      if matched >= 8 {
        matched = 8
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    47 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 44
        _ => break
      }
    }
    48 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 47
        _ => break
      }
    }
    49 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 48
        _ => break
      }
    }
    50 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 48
        _ => break
      }
    }
    51 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 65
        65..=70 => 65
        97..=102 => 65
        _ => break
      }
    }
    52 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 66
        194..=223 => 67
        224..=239 => 68
        240 => 69
        241..=244 => 70
        _ => break
      }
    }
    53 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 52
        _ => break
      }
    }
    54 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 53
        _ => break
      }
    }
    55 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 54
        _ => break
      }
    }
    56 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 54
        _ => break
      }
    }
    57 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 66
        48..=55 => 71
        56..=127 => 66
        194..=223 => 67
        224..=239 => 68
        240 => 69
        241..=244 => 70
        _ => break
      }
    }
    58 => {
      if matched >= 3 {
        matched = 3
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    59 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 58
        _ => break
      }
    }
    60 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 59
        _ => break
      }
    }
    61 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 60
        _ => break
      }
    }
    62 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 60
        _ => break
      }
    }
    63 => {
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 4
      }
      break
    }
    64 => {
      if matched >= 7 {
        matched = 7
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    65 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 72
        65..=70 => 72
        97..=102 => 72
        _ => break
      }
    }
    66 => {
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    67 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 66
        _ => break
      }
    }
    68 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 67
        _ => break
      }
    }
    69 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 68
        _ => break
      }
    }
    70 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 68
        _ => break
      }
    }
    71 => {
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 5
      }
      break
    }
    72 => {
      if matched >= 6 {
        matched = 6
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 6
      }
      break
    }
    _ => panic()
  }

  guard matched <= 13 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_end_pos)
  match matched {
    0 => {
      ()

      if end_with_newline {
        string_repr_buf.write_char('"')
        normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
      } else {
        if not(string_repr_buf.is_empty()) {
          string_interps.val.push(
            InterpLit(
              repr=string_repr_buf.to_string(),
              loc=Location::{ start : env.make_pos(startpos), end : env.make_pos(_end_pos) }
            )
          )
        }
      }
    
    }
    1 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    2 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    3 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    4 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    5 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    6 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    7 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    8 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    9 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if allow_interp {
        if not(string_repr_buf.is_empty()) {
          string_interps.val.push(
            InterpLit(
              repr=string_repr_buf.to_string(),
              loc=Location::{ start : env.make_pos(startpos), end : env.make_pos(_end_pos) }
            )
          )
        }
        string_repr_buf.reset()
        let apos = _end_pos
        let bpos = interp_handle(lexbuf, env~)
        let loc = Location::{ start: env.make_pos(apos), end : env.make_pos(bpos) }
        if string_repr_buf.is_empty() {
          env.add_lexing_error(start=_start_pos, end=_end_pos, InterpMissingExpression)
        } else {
          let source = string_repr_buf.to_string()
          string_interps.val.push(
            InterpSource(
              InterpSource::{ source, loc }
            )
          )
        }
        string_repr_buf.reset()
      } else {
        env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
      }
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    10 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    11 => {
      ()

      env.add_lexing_error(start=_start_pos, end=_end_pos, UnterminatedString)
      if not(string_repr_buf.is_empty()) {
        string_interps.val.push(
          InterpLit(
            repr=string_repr_buf.to_string(),
            loc=Location::{ start : env.make_pos(startpos), end : env.make_pos(_end_pos) }
          )
        )
      }
    
    }
    12 => {
      ()

      // we need insert a NEWLINE token here, so back off to main tokenizer
      lexbuf.reset(pos=lexbuf.curr_pos() - 1)
      if not(end_with_newline) {
        env.add_lexing_error(start=_start_pos, end=_end_pos, UnterminatedString)
      }
      if not(string_repr_buf.is_empty()) {
        string_interps.val.push(
          InterpLit(
            repr=string_repr_buf.to_string(),
            loc=Location::{ start: env.make_pos(startpos), end: env.make_pos(_end_pos) }
          )
        )
      }
    
    }
    13 => {
      ()
      let repr = lexbuf.get_char(_capture_0_start, _capture_0_end)

      string_repr_buf.write_string([repr])
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    _ => panic()
  }
}


fn invalid_byte(lexbuf : Lexbuf, env~ : LexEnv, start~ : Int) -> Unit  {
  // The matched pattern id
  let mut matched = @int.max_value
  let mut _end_pos = -1
  let mut _start_pos = lexbuf.curr_pos()
  let mut _capture_0_start = -1
  let mut _capture_0_end = -1
  let mut _capture_1_start = -1
  let mut _capture_1_end = -1
  loop 0 {
    0 => {
      continue match lexbuf.next_as_int() {
        -1 => 1
        0..=9 => 2
        10 => 1
        11..=12 => 2
        13 => 1
        14..=38 => 2
        39 => 1
        40..=127 => 2
        194..=223 => 3
        224..=236 => 4
        237 => 5
        238..=239 => 4
        240 => 6
        241..=244 => 7
        _ => break
      }
    }
    1 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    2 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    3 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 2
        _ => break
      }
    }
    4 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 3
        _ => break
      }
    }
    5 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 3
        160..=191 => 8
        _ => break
      }
    }
    6 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 4
        _ => break
      }
    }
    7 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 4
        _ => break
      }
    }
    8 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 9
        _ => break
      }
    }
    9 => {
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    _ => panic()
  }

  guard matched <= 2 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_end_pos)
  match matched {
    0 => {
      ()

      env.add_lexing_error(InvalidByteLiteral(invalid_byte_repr_buf.to_string()), start~, end=_end_pos)
      invalid_byte_repr_buf.reset()
    
    }
    1 => {
      ()
      let c = lexbuf.get_char(_capture_0_start, _capture_0_end)

      invalid_byte_repr_buf.write_char(c)
      invalid_byte(lexbuf, env~, start~)
    
    }
    2 => {
      ()

      invalid_byte(lexbuf, env~, start~)
    
    }
    _ => panic()
  }
}


fn tokens(lexbuf : Lexbuf, env~ : LexEnv, preserve_comment~ : (Comment, Int, Int) -> Unit) -> Unit  {
  // The matched pattern id
  let mut matched = @int.max_value
  let mut _end_pos = -1
  let mut _start_pos = lexbuf.curr_pos()
  let mut _capture_0_start = -1
  let mut _capture_0_end = -1
  let mut _capture_1_start = -1
  let mut _capture_1_end = -1
  let mut _capture_2_start = -1
  let mut _capture_2_end = -1
  loop 0 {
    0 => {
      continue match lexbuf.next_as_int() {
        -1 => 1
        0..=8 => 2
        9 => 3
        10 => 4
        11..=12 => 3
        13 => 5
        14..=31 => 2
        32 => 3
        33 => 6
        34 => 7
        35 => 8
        36 => 9
        37 => 10
        38 => 11
        39 => 12
        40 => 13
        41 => 14
        42 => 15
        43 => 16
        44 => 17
        45 => 18
        46 => 19
        47 => 20
        48 => 21
        49..=57 => 22
        58 => 23
        59 => 24
        60 => 25
        61 => 26
        62 => 27
        63 => 28
        64 => 29
        65..=90 => 30
        91 => 31
        92 => 2
        93 => 32
        94 => 33
        95 => 34
        96 => 2
        97 => 34
        98 => 35
        99..=122 => 34
        123 => 36
        124 => 37
        125 => 38
        126..=127 => 2
        194 => 39
        195..=201 => 40
        202 => 41
        203..=223 => 42
        224 => 43
        225 => 44
        226 => 45
        227 => 46
        228..=233 => 47
        234 => 48
        235..=236 => 47
        237 => 49
        238 => 43
        239 => 50
        240 => 51
        241..=242 => 52
        243 => 53
        244 => 52
        _ => break
      }
    }
    1 => {
      if matched >= 66 {
        matched = 66
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    2 => {
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    3 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 3
        11..=12 => 3
        32 => 3
        194 => 132
        225 => 133
        226 => 134
        227 => 135
        239 => 136
        _ => break
      }
    }
    4 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    5 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        10 => 4
        _ => break
      }
    }
    6 => {
      if matched >= 62 {
        matched = 62
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 86
        _ => break
      }
    }
    7 => {
      if matched >= 11 {
        matched = 11
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    8 => {
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        65..=90 => 114
        97..=122 => 114
        124 => 115
        _ => break
      }
    }
    9 => {
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        124 => 116
        _ => break
      }
    }
    10 => {
      if matched >= 30 {
        matched = 30
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 111
        _ => break
      }
    }
    11 => {
      if matched >= 23 {
        matched = 23
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        38 => 112
        _ => break
      }
    }
    12 => {
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 119
        11..=12 => 119
        14..=38 => 119
        40..=91 => 119
        92 => 120
        93..=127 => 119
        194..=223 => 121
        224..=239 => 122
        240 => 123
        241..=244 => 124
        _ => break
      }
    }
    13 => {
      if matched >= 25 {
        matched = 25
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    14 => {
      if matched >= 26 {
        matched = 26
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    15 => {
      if matched >= 28 {
        matched = 28
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 111
        _ => break
      }
    }
    16 => {
      if matched >= 58 {
        matched = 58
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 111
        _ => break
      }
    }
    17 => {
      if matched >= 31 {
        matched = 31
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    18 => {
      if matched >= 59 {
        matched = 59
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 111
        62 => 126
        _ => break
      }
    }
    19 => {
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        40 => 94
        46 => 95
        48..=57 => 96
        65..=90 => 97
        95 => 98
        97..=122 => 98
        194 => 99
        195..=201 => 100
        202 => 101
        225 => 102
        226 => 103
        227 => 104
        228..=233 => 105
        234 => 106
        235..=236 => 105
        237 => 107
        239 => 108
        240 => 109
        243 => 110
        _ => break
      }
    }
    20 => {
      if matched >= 29 {
        matched = 29
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        47 => 125
        61 => 111
        _ => break
      }
    }
    21 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 80
        48..=57 => 22
        66 => 81
        76 => 82
        78 => 82
        79 => 83
        85 => 84
        88 => 85
        95 => 22
        98 => 81
        111 => 83
        120 => 85
        _ => break
      }
    }
    22 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 80
        48..=57 => 22
        76 => 82
        78 => 82
        85 => 84
        95 => 22
        _ => break
      }
    }
    23 => {
      if matched >= 41 {
        matched = 41
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        58 => 93
        _ => break
      }
    }
    24 => {
      if matched >= 42 {
        matched = 42
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    25 => {
      if matched >= 50 {
        matched = 50
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        60 => 91
        61 => 92
        _ => break
      }
    }
    26 => {
      if matched >= 44 {
        matched = 44
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 127
        62 => 128
        _ => break
      }
    }
    27 => {
      if matched >= 48 {
        matched = 48
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 89
        62 => 90
        _ => break
      }
    }
    28 => {
      if matched >= 60 {
        matched = 60
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    29 => {
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        65..=90 => 113
        95 => 113
        97..=122 => 113
        _ => break
      }
    }
    30 => {
      if matched >= 67 {
        matched = 67
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 30
        65..=90 => 30
        95 => 30
        97..=122 => 30
        194 => 68
        195..=201 => 69
        202 => 70
        225 => 71
        226 => 72
        227 => 73
        228..=233 => 74
        234 => 75
        235..=236 => 74
        237 => 76
        239 => 77
        240 => 78
        243 => 79
        _ => break
      }
    }
    31 => {
      if matched >= 51 {
        matched = 51
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    32 => {
      if matched >= 52 {
        matched = 52
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    33 => {
      if matched >= 24 {
        matched = 24
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    34 => {
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 34
        65..=90 => 34
        95 => 34
        97..=122 => 34
        126 => 57
        194 => 58
        195..=201 => 40
        202 => 59
        225 => 60
        226 => 61
        227 => 62
        228..=233 => 47
        234 => 63
        235..=236 => 47
        237 => 64
        239 => 65
        240 => 66
        243 => 67
        _ => break
      }
    }
    35 => {
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        34 => 117
        39 => 118
        48..=57 => 34
        65..=90 => 34
        95 => 34
        97..=122 => 34
        126 => 57
        194 => 58
        195..=201 => 40
        202 => 59
        225 => 60
        226 => 61
        227 => 62
        228..=233 => 47
        234 => 63
        235..=236 => 47
        237 => 64
        239 => 65
        240 => 66
        243 => 67
        _ => break
      }
    }
    36 => {
      if matched >= 53 {
        matched = 53
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    37 => {
      if matched >= 57 {
        matched = 57
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        62 => 87
        124 => 88
        _ => break
      }
    }
    38 => {
      if matched >= 54 {
        matched = 54
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    39 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 2
        160 => 3
        161..=172 => 34
        173 => 2
        174..=191 => 34
        _ => break
      }
    }
    40 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 34
        _ => break
      }
    }
    41 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 34
        176..=191 => 2
        _ => break
      }
    }
    42 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 2
        _ => break
      }
    }
    43 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 42
        _ => break
      }
    }
    44 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 42
        132..=135 => 40
        136..=153 => 42
        154 => 131
        155..=183 => 42
        184..=187 => 40
        188..=191 => 42
        _ => break
      }
    }
    45 => {
      continue match lexbuf.next_as_int() {
        128 => 137
        129 => 138
        130 => 40
        131..=132 => 42
        133..=134 => 40
        135..=185 => 42
        186..=187 => 40
        188..=190 => 42
        191 => 139
        _ => break
      }
    }
    46 => {
      continue match lexbuf.next_as_int() {
        128 => 130
        129..=131 => 40
        132..=134 => 42
        135..=191 => 40
        _ => break
      }
    }
    47 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 40
        _ => break
      }
    }
    48 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 42
        176..=191 => 40
        _ => break
      }
    }
    49 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 40
        160..=191 => 42
        _ => break
      }
    }
    50 => {
      continue match lexbuf.next_as_int() {
        128..=163 => 42
        164..=171 => 40
        172..=183 => 42
        184..=185 => 40
        186 => 42
        187 => 129
        188..=191 => 42
        _ => break
      }
    }
    51 => {
      continue match lexbuf.next_as_int() {
        144..=158 => 43
        159 => 55
        160..=174 => 47
        175 => 56
        176..=178 => 47
        179..=191 => 43
        _ => break
      }
    }
    52 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 43
        _ => break
      }
    }
    53 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 43
        160 => 54
        161..=191 => 43
        _ => break
      }
    }
    54 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 42
        132..=135 => 40
        136..=191 => 42
        _ => break
      }
    }
    55 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 40
        176..=191 => 42
        _ => break
      }
    }
    56 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 42
        160..=168 => 40
        169..=191 => 42
        _ => break
      }
    }
    57 => {
      if matched >= 68 {
        matched = 68
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos + -1
      }
      break
    }
    58 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 34
        174..=191 => 34
        _ => break
      }
    }
    59 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 34
        _ => break
      }
    }
    60 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 40
        184..=187 => 40
        _ => break
      }
    }
    61 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 40
        133..=134 => 40
        186..=187 => 40
        191 => 143
        _ => break
      }
    }
    62 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 40
        135..=191 => 40
        _ => break
      }
    }
    63 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 40
        _ => break
      }
    }
    64 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 40
        _ => break
      }
    }
    65 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 40
        184..=185 => 40
        _ => break
      }
    }
    66 => {
      continue match lexbuf.next_as_int() {
        159 => 141
        160..=174 => 47
        175 => 142
        176..=178 => 47
        _ => break
      }
    }
    67 => {
      continue match lexbuf.next_as_int() {
        160 => 140
        _ => break
      }
    }
    68 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 30
        174..=191 => 30
        _ => break
      }
    }
    69 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 30
        _ => break
      }
    }
    70 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 30
        _ => break
      }
    }
    71 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 69
        184..=187 => 69
        _ => break
      }
    }
    72 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 69
        133..=134 => 69
        186..=187 => 69
        191 => 147
        _ => break
      }
    }
    73 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 69
        135..=191 => 69
        _ => break
      }
    }
    74 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 69
        _ => break
      }
    }
    75 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 69
        _ => break
      }
    }
    76 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 69
        _ => break
      }
    }
    77 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 69
        184..=185 => 69
        _ => break
      }
    }
    78 => {
      continue match lexbuf.next_as_int() {
        159 => 145
        160..=174 => 74
        175 => 146
        176..=178 => 74
        _ => break
      }
    }
    79 => {
      continue match lexbuf.next_as_int() {
        160 => 144
        _ => break
      }
    }
    80 => {
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 148
        48..=57 => 149
        69 => 150
        95 => 149
        101 => 150
        _ => break
      }
    }
    81 => {
      continue match lexbuf.next_as_int() {
        48..=49 => 152
        _ => break
      }
    }
    82 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 151
        _ => break
      }
    }
    83 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 153
        _ => break
      }
    }
    84 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 151
        76 => 82
        _ => break
      }
    }
    85 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 154
        65..=70 => 154
        97..=102 => 154
        _ => break
      }
    }
    86 => {
      if matched >= 61 {
        matched = 61
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    87 => {
      if matched >= 55 {
        matched = 55
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    88 => {
      if matched >= 56 {
        matched = 56
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    89 => {
      if matched >= 46 {
        matched = 46
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    90 => {
      if matched >= 47 {
        matched = 47
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    91 => {
      if matched >= 49 {
        matched = 49
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    92 => {
      if matched >= 45 {
        matched = 45
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    93 => {
      if matched >= 40 {
        matched = 40
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    94 => {
      if matched >= 32 {
        matched = 32
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    95 => {
      if matched >= 37 {
        matched = 37
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        46 => 171
        60 => 172
        61 => 173
        _ => break
      }
    }
    96 => {
      if matched >= 33 {
        matched = 33
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
        _capture_1_start = _start_pos + 1
        _capture_1_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 96
        _ => break
      }
    }
    97 => {
      if matched >= 38 {
        matched = 38
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 1
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 97
        65..=90 => 97
        95 => 97
        97..=122 => 97
        194 => 159
        195..=201 => 160
        202 => 161
        225 => 162
        226 => 163
        227 => 164
        228..=233 => 165
        234 => 166
        235..=236 => 165
        237 => 167
        239 => 168
        240 => 169
        243 => 170
        _ => break
      }
    }
    98 => {
      if matched >= 39 {
        matched = 39
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 1
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 98
        65..=90 => 98
        95 => 98
        97..=122 => 98
        194 => 99
        195..=201 => 100
        202 => 101
        225 => 102
        226 => 103
        227 => 104
        228..=233 => 105
        234 => 106
        235..=236 => 105
        237 => 107
        239 => 108
        240 => 109
        243 => 110
        _ => break
      }
    }
    99 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 98
        174..=191 => 98
        _ => break
      }
    }
    100 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 98
        _ => break
      }
    }
    101 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 98
        _ => break
      }
    }
    102 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 100
        184..=187 => 100
        _ => break
      }
    }
    103 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 100
        133..=134 => 100
        186..=187 => 100
        191 => 158
        _ => break
      }
    }
    104 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 100
        135..=191 => 100
        _ => break
      }
    }
    105 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 100
        _ => break
      }
    }
    106 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 100
        _ => break
      }
    }
    107 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 100
        _ => break
      }
    }
    108 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 100
        184..=185 => 100
        _ => break
      }
    }
    109 => {
      continue match lexbuf.next_as_int() {
        159 => 156
        160..=174 => 105
        175 => 157
        176..=178 => 105
        _ => break
      }
    }
    110 => {
      continue match lexbuf.next_as_int() {
        160 => 155
        _ => break
      }
    }
    111 => {
      if matched >= 27 {
        matched = 27
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 1
        _capture_1_start = _start_pos
        _capture_1_end = _start_pos + 2
      }
      break
    }
    112 => {
      if matched >= 22 {
        matched = 22
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    113 => {
      if matched >= 16 {
        matched = 16
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 1
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        47 => 174
        48..=57 => 113
        65..=90 => 113
        95 => 113
        97..=122 => 113
        _ => break
      }
    }
    114 => {
      if matched >= 15 {
        matched = 15
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 1
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 114
        11..=12 => 114
        14..=127 => 114
        194..=223 => 175
        224..=239 => 176
        240 => 177
        241..=244 => 178
        _ => break
      }
    }
    115 => {
      if matched >= 14 {
        matched = 14
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 2
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 115
        11..=12 => 115
        14..=127 => 115
        194..=223 => 179
        224..=239 => 180
        240 => 181
        241..=244 => 182
        _ => break
      }
    }
    116 => {
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    117 => {
      if matched >= 12 {
        matched = 12
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    118 => {
      if matched >= 21 {
        matched = 21
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 2
      }
      continue match lexbuf.next_as_int() {
        0..=91 => 183
        92 => 184
        93..=127 => 183
        _ => break
      }
    }
    119 => {
      continue match lexbuf.next_as_int() {
        39 => 189
        _ => break
      }
    }
    120 => {
      continue match lexbuf.next_as_int() {
        32 => 185
        34 => 185
        39 => 185
        92 => 185
        98 => 185
        110 => 185
        111 => 186
        114 => 185
        116 => 185
        117 => 187
        120 => 188
        _ => break
      }
    }
    121 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 119
        _ => break
      }
    }
    122 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 121
        _ => break
      }
    }
    123 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 122
        _ => break
      }
    }
    124 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 122
        _ => break
      }
    }
    125 => {
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 125
        11..=12 => 125
        14..=127 => 125
        194..=223 => 190
        224..=239 => 191
        240 => 192
        241..=244 => 193
        _ => break
      }
    }
    126 => {
      if matched >= 3 {
        matched = 3
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    127 => {
      if matched >= 43 {
        matched = 43
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    128 => {
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    129 => {
      continue match lexbuf.next_as_int() {
        128..=190 => 2
        191 => 3
        _ => break
      }
    }
    130 => {
      continue match lexbuf.next_as_int() {
        128 => 194
        129..=191 => 34
        _ => break
      }
    }
    131 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        129..=191 => 2
        _ => break
      }
    }
    132 => {
      continue match lexbuf.next_as_int() {
        160 => 3
        _ => break
      }
    }
    133 => {
      continue match lexbuf.next_as_int() {
        154 => 196
        _ => break
      }
    }
    134 => {
      continue match lexbuf.next_as_int() {
        128 => 197
        129 => 198
        _ => break
      }
    }
    135 => {
      continue match lexbuf.next_as_int() {
        128 => 196
        _ => break
      }
    }
    136 => {
      continue match lexbuf.next_as_int() {
        187 => 195
        _ => break
      }
    }
    137 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 3
        139..=167 => 2
        168..=169 => 4
        170..=174 => 2
        175 => 3
        176..=191 => 2
        _ => break
      }
    }
    138 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 34
        159 => 194
        160..=191 => 34
        _ => break
      }
    }
    139 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 2
        176..=191 => 34
        _ => break
      }
    }
    140 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 40
        _ => break
      }
    }
    141 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 40
        _ => break
      }
    }
    142 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 40
        _ => break
      }
    }
    143 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 34
        _ => break
      }
    }
    144 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 69
        _ => break
      }
    }
    145 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 69
        _ => break
      }
    }
    146 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 69
        _ => break
      }
    }
    147 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 30
        _ => break
      }
    }
    148 => {
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos + -2
      }
      break
    }
    149 => {
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 149
        69 => 150
        95 => 149
        101 => 150
        _ => break
      }
    }
    150 => {
      continue match lexbuf.next_as_int() {
        43 => 199
        45 => 199
        48..=57 => 200
        _ => break
      }
    }
    151 => {
      continue match lexbuf.next_as_int() {
        46 => 148
        _ => break
      }
    }
    152 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 151
        48..=49 => 152
        76 => 82
        78 => 82
        85 => 84
        95 => 152
        _ => break
      }
    }
    153 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 151
        48..=55 => 153
        76 => 82
        78 => 82
        85 => 84
        95 => 153
        _ => break
      }
    }
    154 => {
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 201
        48..=57 => 154
        65..=70 => 154
        76 => 82
        78 => 82
        85 => 84
        95 => 154
        97..=102 => 154
        _ => break
      }
    }
    155 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 100
        _ => break
      }
    }
    156 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 100
        _ => break
      }
    }
    157 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 100
        _ => break
      }
    }
    158 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 98
        _ => break
      }
    }
    159 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 97
        174..=191 => 97
        _ => break
      }
    }
    160 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 97
        _ => break
      }
    }
    161 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 97
        _ => break
      }
    }
    162 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 160
        184..=187 => 160
        _ => break
      }
    }
    163 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 160
        133..=134 => 160
        186..=187 => 160
        191 => 205
        _ => break
      }
    }
    164 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 160
        135..=191 => 160
        _ => break
      }
    }
    165 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 160
        _ => break
      }
    }
    166 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 160
        _ => break
      }
    }
    167 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 160
        _ => break
      }
    }
    168 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 160
        184..=185 => 160
        _ => break
      }
    }
    169 => {
      continue match lexbuf.next_as_int() {
        159 => 203
        160..=174 => 165
        175 => 204
        176..=178 => 165
        _ => break
      }
    }
    170 => {
      continue match lexbuf.next_as_int() {
        160 => 202
        _ => break
      }
    }
    171 => {
      if matched >= 34 {
        matched = 34
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    172 => {
      if matched >= 36 {
        matched = 36
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    173 => {
      if matched >= 35 {
        matched = 35
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    174 => {
      continue match lexbuf.next_as_int() {
        65..=90 => 113
        95 => 113
        97..=122 => 113
        _ => break
      }
    }
    175 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 114
        _ => break
      }
    }
    176 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 175
        _ => break
      }
    }
    177 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 176
        _ => break
      }
    }
    178 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 176
        _ => break
      }
    }
    179 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 115
        _ => break
      }
    }
    180 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 179
        _ => break
      }
    }
    181 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 180
        _ => break
      }
    }
    182 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 180
        _ => break
      }
    }
    183 => {
      continue match lexbuf.next_as_int() {
        39 => 206
        _ => break
      }
    }
    184 => {
      continue match lexbuf.next_as_int() {
        32 => 207
        34 => 207
        39 => 208
        92 => 207
        98 => 207
        110 => 207
        111 => 209
        114 => 207
        116 => 207
        120 => 210
        _ => break
      }
    }
    185 => {
      continue match lexbuf.next_as_int() {
        39 => 215
        _ => break
      }
    }
    186 => {
      continue match lexbuf.next_as_int() {
        48..=51 => 213
        _ => break
      }
    }
    187 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 211
        65..=70 => 211
        97..=102 => 211
        123 => 212
        _ => break
      }
    }
    188 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 214
        65..=70 => 214
        97..=102 => 214
        _ => break
      }
    }
    189 => {
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      break
    }
    190 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 125
        _ => break
      }
    }
    191 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 190
        _ => break
      }
    }
    192 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 191
        _ => break
      }
    }
    193 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 191
        _ => break
      }
    }
    194 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 3
        11..=12 => 3
        32 => 3
        48..=57 => 34
        65..=90 => 34
        95 => 34
        97..=122 => 34
        126 => 57
        194 => 216
        195..=201 => 40
        202 => 59
        225 => 217
        226 => 218
        227 => 219
        228..=233 => 47
        234 => 63
        235..=236 => 47
        237 => 64
        239 => 220
        240 => 66
        243 => 67
        _ => break
      }
    }
    195 => {
      continue match lexbuf.next_as_int() {
        191 => 3
        _ => break
      }
    }
    196 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        _ => break
      }
    }
    197 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 3
        175 => 3
        _ => break
      }
    }
    198 => {
      continue match lexbuf.next_as_int() {
        159 => 3
        _ => break
      }
    }
    199 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 200
        _ => break
      }
    }
    200 => {
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 200
        95 => 200
        _ => break
      }
    }
    201 => {
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        46 => 148
        48..=57 => 221
        65..=70 => 221
        80 => 150
        95 => 221
        97..=102 => 221
        112 => 150
        _ => break
      }
    }
    202 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 160
        _ => break
      }
    }
    203 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 160
        _ => break
      }
    }
    204 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 160
        _ => break
      }
    }
    205 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 97
        _ => break
      }
    }
    206 => {
      if matched >= 19 {
        matched = 19
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 2
        _capture_0_end = _start_pos + 3
      }
      break
    }
    207 => {
      continue match lexbuf.next_as_int() {
        39 => 222
        _ => break
      }
    }
    208 => {
      if matched >= 19 {
        matched = 19
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 2
        _capture_0_end = _start_pos + 3
      }
      continue match lexbuf.next_as_int() {
        39 => 222
        _ => break
      }
    }
    209 => {
      continue match lexbuf.next_as_int() {
        48..=51 => 223
        _ => break
      }
    }
    210 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 224
        65..=70 => 224
        97..=102 => 224
        _ => break
      }
    }
    211 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 226
        65..=70 => 226
        97..=102 => 226
        _ => break
      }
    }
    212 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 212
        65..=70 => 212
        97..=102 => 212
        125 => 225
        _ => break
      }
    }
    213 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 227
        _ => break
      }
    }
    214 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 228
        65..=70 => 228
        97..=102 => 228
        _ => break
      }
    }
    215 => {
      if matched >= 6 {
        matched = 6
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 4
      }
      break
    }
    216 => {
      continue match lexbuf.next_as_int() {
        160 => 3
        161..=172 => 34
        174..=191 => 34
        _ => break
      }
    }
    217 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 40
        154 => 196
        184..=187 => 40
        _ => break
      }
    }
    218 => {
      continue match lexbuf.next_as_int() {
        128 => 197
        129 => 138
        130 => 40
        133..=134 => 40
        186..=187 => 40
        191 => 143
        _ => break
      }
    }
    219 => {
      continue match lexbuf.next_as_int() {
        128 => 130
        129..=131 => 40
        135..=191 => 40
        _ => break
      }
    }
    220 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 40
        184..=185 => 40
        187 => 195
        _ => break
      }
    }
    221 => {
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _end_pos
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 221
        65..=70 => 221
        80 => 150
        95 => 221
        97..=102 => 221
        112 => 150
        _ => break
      }
    }
    222 => {
      if matched >= 20 {
        matched = 20
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 3
        _capture_0_end = _start_pos + 4
      }
      break
    }
    223 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 229
        _ => break
      }
    }
    224 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 230
        65..=70 => 230
        97..=102 => 230
        _ => break
      }
    }
    225 => {
      continue match lexbuf.next_as_int() {
        39 => 231
        _ => break
      }
    }
    226 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 232
        65..=70 => 232
        97..=102 => 232
        _ => break
      }
    }
    227 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 233
        _ => break
      }
    }
    228 => {
      continue match lexbuf.next_as_int() {
        39 => 234
        _ => break
      }
    }
    229 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 235
        _ => break
      }
    }
    230 => {
      continue match lexbuf.next_as_int() {
        39 => 236
        _ => break
      }
    }
    231 => {
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 4
        _capture_0_end = _end_pos + -2
        _capture_1_start = _start_pos
        _capture_1_end = _end_pos
      }
      break
    }
    232 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 237
        65..=70 => 237
        97..=102 => 237
        _ => break
      }
    }
    233 => {
      continue match lexbuf.next_as_int() {
        39 => 238
        _ => break
      }
    }
    234 => {
      if matched >= 7 {
        matched = 7
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 6
      }
      break
    }
    235 => {
      continue match lexbuf.next_as_int() {
        39 => 239
        _ => break
      }
    }
    236 => {
      if matched >= 17 {
        matched = 17
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 4
        _capture_0_end = _start_pos + 6
      }
      break
    }
    237 => {
      continue match lexbuf.next_as_int() {
        39 => 240
        _ => break
      }
    }
    238 => {
      if matched >= 8 {
        matched = 8
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 7
      }
      break
    }
    239 => {
      if matched >= 18 {
        matched = 18
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos + 4
        _capture_0_end = _start_pos + 7
      }
      break
    }
    240 => {
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _start_pos
        _capture_0_end = _start_pos + 8
      }
      break
    }
    _ => panic()
  }

  guard matched <= 70 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_end_pos)
  match matched {
    0 => {
      ()

      env.add_token_with_loc(NEWLINE, start=_start_pos, end=_end_pos)
      env.current_bol = _end_pos
      env.current_line += 1
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    1 => {
      ()

      tokens(lexbuf, env~, preserve_comment~)
    
    }
    2 => {
      ()

      env.add_token_with_loc(FAT_ARROW, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    3 => {
      ()

      env.add_token_with_loc(THIN_ARROW, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    4 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos, end = _end_pos,InterpInvalidComment)
      }
      if env.comment {
        let comment = Comment::{
          content : repr,
          kind : InlineTrailing,
          consumed_by_docstring : @ref.new(false)
        }
        preserve_comment(comment, _start_pos, _end_pos)
        env.add_token_with_loc(COMMENT(comment), start=_start_pos, end=_end_pos)
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    5 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    6 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    7 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    8 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    9 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    10 => {
      ()
      let hex = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let repr = lexbuf.get_string(_capture_1_start, _capture_1_end)
      let _ = repr

      if char_for_hex_escape(hex) is None {
        // Overflow
        env.add_lexing_error(start=_capture_1_start, end = _capture_1_end, InvalidEscapeSequence(repr))
      }
      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    11 => {
      ()

      let startpos = _start_pos
      let tok : Token =
        match string(lexbuf, env~, end_with_newline = false, allow_interp = true, startpos~) {
          [ InterpLit(repr~, ..) ] => STRING(repr)
          interps => INTERP(interps)
        }
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_pos(startpos), env.make_pos(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    12 => {
      ()

      let startpos = _start_pos
      let tok : Token =
        match string(lexbuf, env~, end_with_newline = false, allow_interp = false, startpos~) {
          [ InterpLit(repr~, ..) ] => STRING(repr)
          _interps => panic()
        }
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_pos(startpos), env.make_pos(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    13 => {
      ()

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos, end = _end_pos, InterpInvalidMultilineString)
      }
      let startpos = _start_pos
      let tok : Token = MULTILINE_INTERP(string(lexbuf, env~, end_with_newline = true, allow_interp = true, startpos~))
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_pos(startpos), env.make_pos(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    14 => {
      ()
      let s = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos, end=_end_pos, InterpInvalidMultilineString)
      }
      env.add_token_with_loc(MULTILINE_STRING(s), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    15 => {
      ()
      let attr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos, end=_end_pos, InterpInvalidAttribute)
      }
      env.add_token_with_loc(ATTRIBUTE(attr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    16 => {
      ()
      let pkgname = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(PACKAGE_NAME(pkgname), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    17 => {
      ()
      let hex = lexbuf.get_string(_capture_0_start, _capture_0_end)

      let literal = "\\x" + hex
      env.add_token_with_loc(BYTE(literal), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    18 => {
      ()
      let oct = lexbuf.get_string(_capture_0_start, _capture_0_end)

      let literal = "\\o" + oct
      env.add_token_with_loc(BYTE(literal), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    19 => {
      ()
      let ascii = lexbuf.get_char(_capture_0_start, _capture_0_end)

      let literal = ascii
      env.add_token_with_loc(BYTE([literal]), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    20 => {
      ()
      let e = lexbuf.get_char(_capture_0_start, _capture_0_end)

      let literal = "\\" + [e]
      env.add_token_with_loc(BYTE(literal), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    21 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = repr

      invalid_byte(lexbuf, env~, start=_capture_0_start)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    22 => {
      ()

      env.add_token_with_loc(AMPERAMPER, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    23 => {
      ()

      env.add_token_with_loc(AMPER, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    24 => {
      ()

      env.add_token_with_loc(CARET, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    25 => {
      ()

      env.add_token_with_loc(LPAREN, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    26 => {
      ()

      env.add_token_with_loc(RPAREN, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    27 => {
      ()
      let op = lexbuf.get_char(_capture_0_start, _capture_0_end)
      let repr = lexbuf.get_string(_capture_1_start, _capture_1_end)
      let _ = repr

      env.add_token_with_loc(AUGMENTED_ASSIGNMENT([op]), start=_capture_1_start, end=_capture_1_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    28 => {
      ()

      env.add_token_with_loc(INFIX3("*"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    29 => {
      ()

      env.add_token_with_loc(INFIX3("/"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    30 => {
      ()

      env.add_token_with_loc(INFIX3("%"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    31 => {
      ()

      env.add_token_with_loc(COMMA, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    32 => {
      ()

      env.add_token_with_loc(DOT_LPAREN, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    33 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = repr
      let digits = lexbuf.get_string(_capture_1_start, _capture_1_end)

      let idx =
        try {
          @strconv.parse_int!(digits)
        } catch {
          StrConvError(_) => {
            env.add_lexing_error(InvalidDotInt(repr), start=_capture_0_start, end=_capture_0_end)
            0
          }
        }
      env.add_token_with_loc(DOT_INT(idx), start=_capture_0_start, end=_capture_0_end, start_offset = 1)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    34 => {
      ()

      env.add_token_with_loc(ELLIPSIS, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    35 => {
      ()

      env.add_token_with_loc(RANGE_INCLUSIVE, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    36 => {
      ()

      env.add_token_with_loc(RANGE_EXCLUSIVE, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    37 => {
      ()

      env.add_token_with_loc(DOTDOT, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    38 => {
      ()
      let name = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = name

      env.add_token_with_loc(DOT_UIDENT(name), start=_capture_0_start, end=_capture_0_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    39 => {
      ()
      let name = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = name

      env.add_token_with_loc(DOT_LIDENT(name), start=_capture_0_start, end=_capture_0_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    40 => {
      ()

      env.add_token_with_loc(COLONCOLON, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    41 => {
      ()

      env.add_token_with_loc(COLON, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    42 => {
      ()

      env.add_token_with_loc(SEMI(true), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    43 => {
      ()

      env.add_token_with_loc(INFIX1("=="), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    44 => {
      ()

      env.add_token_with_loc(EQUAL, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    45 => {
      ()

      env.add_token_with_loc(INFIX1("<="), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    46 => {
      ()

      env.add_token_with_loc(INFIX1(">="), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    47 => {
      ()

      env.add_token_with_loc(INFIX2(">>"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    48 => {
      ()

      env.add_token_with_loc(INFIX1(">"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    49 => {
      ()

      env.add_token_with_loc(INFIX2("<<"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    50 => {
      ()

      env.add_token_with_loc(INFIX1("<"), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    51 => {
      ()

      env.add_token_with_loc(LBRACKET, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    52 => {
      ()

      env.add_token_with_loc(RBRACKET, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    53 => {
      ()

      env.add_token_with_loc(LBRACE, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    54 => {
      ()

      env.add_token_with_loc(RBRACE, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    55 => {
      ()

      env.add_token_with_loc(PIPE, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    56 => {
      ()

      env.add_token_with_loc(BARBAR, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    57 => {
      ()

      env.add_token_with_loc(BAR, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    58 => {
      ()

      env.add_token_with_loc(PLUS, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    59 => {
      ()

      env.add_token_with_loc(MINUS, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    60 => {
      ()

      env.add_token_with_loc(QUESTION, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    61 => {
      ()

      env.add_token_with_loc(INFIX1("!="), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    62 => {
      ()

      env.add_token_with_loc(EXCLAMATION, start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    63 => {
      ()
      let integer = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = integer

      lexbuf.reset(pos=_capture_0_end)
      env.add_token_with_loc(INT(integer), start=_capture_0_start, end=_capture_0_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    64 => {
      ()
      let float = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(FLOAT(float), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    65 => {
      ()
      let integer = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(INT(integer), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    66 => {
      ()

      let end = lexbuf.curr_pos()
      env.add_token_with_loc(EOF, start=end, end=end)
    
    }
    67 => {
      ()
      let raw = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(UIDENT(raw), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    68 => {
      ()
      let raw = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(POST_LABEL(raw), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    69 => {
      ()
      let raw = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if reserved_keyword_table.contains(raw) {
        env.add_lexing_error(Reserved_keyword(raw), start=_start_pos, end=_end_pos)
      }
      env.add_token_with_loc(
        match keyword_table[raw] {
          None => LIDENT(raw)
          Some(tok) => tok
        },
        start=_start_pos, end=_end_pos
      )
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    70 => {
      ()
      let c = lexbuf.get_char(_capture_0_start, _capture_0_end)

      env.add_lexing_error(IllegalCharacter(c), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    _ => panic()
  }
}





