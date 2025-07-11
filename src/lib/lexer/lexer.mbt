
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
  let mut _match_pattern = @int.max_value
  let mut _match_start = lexbuf.curr_pos()
  let mut _match_end = -1
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
      _match_pattern = 1
      _match_end = lexbuf.curr_pos()
      break
    }
    2 => {
      _match_pattern = 4
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    3 => {
      _match_pattern = 4
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 2
      _match_end = lexbuf.curr_pos()
      break
    }
    5 => {
      _match_pattern = 5
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    6 => {
      _match_pattern = 0
      _match_end = lexbuf.curr_pos()
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

  guard _match_pattern <= 5 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_match_end)
  match _match_pattern {
    0 => {
      ()

      _match_start
    
    }
    1 => {
      ()

      env.add_lexing_error(start=_match_start, end = _match_end, UnterminatedString)
      _match_start
    
    }
    2 => {
      ()

      env.add_lexing_error(start=_match_start, end = _match_end, UnterminatedStringInVariableInterploation)
      lexbuf.reset(pos=_match_start)
      _match_start
    
    }
    3 => {
      ()

      env.add_lexing_error(start=_match_start, end = _match_end, UnterminatedStringInVariableInterploation)
      _match_start
    
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
      env.add_lexing_error(start=_match_start, end = _match_end, IllegalCharacter(c))
      interp_handle(lexbuf, env~)
    
    }
    _ => panic()
  }
}


fn normal(lexbuf : Lexbuf, env~ : LexEnv, end_with_newline~ : Bool, allow_interp~ : Bool, startpos~ : Int) -> Unit  {
  // The matched pattern id
  let mut _match_pattern = @int.max_value
  let mut _match_start = lexbuf.curr_pos()
  let mut _match_end = -1
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
      _match_pattern = 11
      _match_end = lexbuf.curr_pos()
      break
    }
    2 => {
      _match_pattern = 13
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    3 => {
      _match_pattern = 13
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        10 => 10
        _ => break
      }
    }
    4 => {
      _match_pattern = 0
      _match_end = lexbuf.curr_pos()
      break
    }
    5 => {
      _match_pattern = 13
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 12
      _match_end = lexbuf.curr_pos()
      break
    }
    11 => {
      _match_pattern = 10
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    12 => {
      _match_pattern = 1
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 2
      break
    }
    13 => {
      _match_pattern = 10
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 10
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        48..=57 => 26
        65..=70 => 26
        97..=102 => 26
        123 => 27
        _ => break
      }
    }
    15 => {
      _match_pattern = 10
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 9
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 8
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 3
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 2
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 4
      break
    }
    64 => {
      _match_pattern = 7
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 5
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 4
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 5
      break
    }
    72 => {
      _match_pattern = 6
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 6
      break
    }
    _ => panic()
  }

  guard _match_pattern <= 13 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_match_end)
  match _match_pattern {
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
              loc=Location::{ start : env.make_pos(startpos), end : env.make_pos(_match_end) }
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

      env.add_lexing_error(start=_match_start, end=_match_end, InvalidEscapeSequence(repr))
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

      env.add_lexing_error(start=_match_start, end=_match_end, InvalidEscapeSequence(repr))
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

      env.add_lexing_error(start=_match_start, end=_match_end, InvalidEscapeSequence(repr))
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
              loc=Location::{ start : env.make_pos(startpos), end : env.make_pos(_match_end) }
            )
          )
        }
        string_repr_buf.reset()
        let apos = _match_end
        let bpos = interp_handle(lexbuf, env~)
        let loc = Location::{ start: env.make_pos(apos), end : env.make_pos(bpos) }
        if string_repr_buf.is_empty() {
          env.add_lexing_error(start=_match_start, end=_match_end, InterpMissingExpression)
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
        env.add_lexing_error(start=_match_start, end=_match_end, InvalidEscapeSequence(repr))
      }
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    10 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_lexing_error(start=_match_start, end=_match_end, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    11 => {
      ()

      env.add_lexing_error(start=_match_start, end=_match_end, UnterminatedString)
      if not(string_repr_buf.is_empty()) {
        string_interps.val.push(
          InterpLit(
            repr=string_repr_buf.to_string(),
            loc=Location::{ start : env.make_pos(startpos), end : env.make_pos(_match_end) }
          )
        )
      }
    
    }
    12 => {
      ()

      // we need insert a NEWLINE token here, so back off to main tokenizer
      lexbuf.reset(pos=lexbuf.curr_pos() - 1)
      if not(end_with_newline) {
        env.add_lexing_error(start=_match_start, end=_match_end, UnterminatedString)
      }
      if not(string_repr_buf.is_empty()) {
        string_interps.val.push(
          InterpLit(
            repr=string_repr_buf.to_string(),
            loc=Location::{ start: env.make_pos(startpos), end: env.make_pos(_match_end) }
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
  let mut _match_pattern = @int.max_value
  let mut _match_start = lexbuf.curr_pos()
  let mut _match_end = -1
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
      _match_pattern = 0
      _match_end = lexbuf.curr_pos()
      break
    }
    2 => {
      _match_pattern = 1
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 2
      _match_end = lexbuf.curr_pos()
      break
    }
    _ => panic()
  }

  guard _match_pattern <= 2 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_match_end)
  match _match_pattern {
    0 => {
      ()

      env.add_lexing_error(InvalidByteLiteral(invalid_byte_repr_buf.to_string()), start~, end=_match_end)
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
  let mut _match_pattern = @int.max_value
  let mut _match_start = lexbuf.curr_pos()
  let mut _match_end = -1
  let mut _capture_0_start = -1
  let mut _capture_0_end = -1
  let mut _capture_1_start = -1
  let mut _capture_1_end = -1
  let mut _capture_2_start = -1
  let mut _capture_2_end = -1
  let mut _capture_3_start = -1
  let mut _capture_3_end = -1
  let mut _tag_0 = -1
  let mut _tag_1 = -1
  let mut _tag_1_1 = -1
  let mut _tag_2 = -1
  let mut _tag_3 = -1
  let mut _tag_3_1 = -1
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
      _match_pattern = 70
      _match_end = lexbuf.curr_pos()
      break
    }
    2 => {
      _match_pattern = 74
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    3 => {
      _match_pattern = 1
      _match_end = lexbuf.curr_pos()
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
      _match_pattern = 0
      _match_end = lexbuf.curr_pos()
      break
    }
    5 => {
      _match_pattern = 0
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        10 => 4
        _ => break
      }
    }
    6 => {
      _match_pattern = 65
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 86
        _ => break
      }
    }
    7 => {
      _match_pattern = 11
      _match_end = lexbuf.curr_pos()
      break
    }
    8 => {
      _match_pattern = 74
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        65..=90 => 114
        95 => 114
        97..=122 => 114
        124 => 115
        _ => break
      }
    }
    9 => {
      _match_pattern = 74
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        124 => 116
        _ => break
      }
    }
    10 => {
      _match_pattern = 33
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 111
        _ => break
      }
    }
    11 => {
      _match_pattern = 26
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        38 => 112
        _ => break
      }
    }
    12 => {
      _match_pattern = 74
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 28
      _match_end = lexbuf.curr_pos()
      break
    }
    14 => {
      _match_pattern = 29
      _match_end = lexbuf.curr_pos()
      break
    }
    15 => {
      _match_pattern = 31
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 111
        _ => break
      }
    }
    16 => {
      _match_pattern = 61
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 111
        _ => break
      }
    }
    17 => {
      _match_pattern = 34
      _match_end = lexbuf.curr_pos()
      break
    }
    18 => {
      _match_pattern = 62
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 111
        62 => 126
        _ => break
      }
    }
    19 => {
      _match_pattern = 74
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 32
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        47 => 125
        61 => 111
        _ => break
      }
    }
    21 => {
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 44
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        58 => 93
        _ => break
      }
    }
    24 => {
      _match_pattern = 45
      _match_end = lexbuf.curr_pos()
      break
    }
    25 => {
      _match_pattern = 53
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        60 => 91
        61 => 92
        _ => break
      }
    }
    26 => {
      _match_pattern = 47
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 127
        62 => 128
        _ => break
      }
    }
    27 => {
      _match_pattern = 51
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        61 => 89
        62 => 90
        _ => break
      }
    }
    28 => {
      _match_pattern = 63
      _match_end = lexbuf.curr_pos()
      break
    }
    29 => {
      _match_pattern = 74
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        65..=90 => 113
        95 => 113
        97..=122 => 113
        _ => break
      }
    }
    30 => {
      _match_pattern = 71
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 54
      _match_end = lexbuf.curr_pos()
      break
    }
    32 => {
      _match_pattern = 55
      _match_end = lexbuf.curr_pos()
      break
    }
    33 => {
      _match_pattern = 27
      _match_end = lexbuf.curr_pos()
      break
    }
    34 => {
      _match_pattern = 73
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 73
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
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
      _match_pattern = 56
      _match_end = lexbuf.curr_pos()
      break
    }
    37 => {
      _match_pattern = 60
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        62 => 87
        124 => 88
        _ => break
      }
    }
    38 => {
      _match_pattern = 57
      _match_end = lexbuf.curr_pos()
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
      _match_pattern = 72
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end + -1
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
      _match_pattern = 68
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 148
        48..=57 => 149
        69 => 150
        70 => 151
        95 => 149
        101 => 150
        _ => break
      }
    }
    81 => {
      continue match lexbuf.next_as_int() {
        48..=49 => 153
        _ => break
      }
    }
    82 => {
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 152
        _ => break
      }
    }
    83 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 154
        _ => break
      }
    }
    84 => {
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 152
        76 => 82
        _ => break
      }
    }
    85 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 155
        65..=70 => 155
        97..=102 => 155
        _ => break
      }
    }
    86 => {
      _match_pattern = 64
      _match_end = lexbuf.curr_pos()
      break
    }
    87 => {
      _match_pattern = 58
      _match_end = lexbuf.curr_pos()
      break
    }
    88 => {
      _match_pattern = 59
      _match_end = lexbuf.curr_pos()
      break
    }
    89 => {
      _match_pattern = 49
      _match_end = lexbuf.curr_pos()
      break
    }
    90 => {
      _match_pattern = 50
      _match_end = lexbuf.curr_pos()
      break
    }
    91 => {
      _match_pattern = 52
      _match_end = lexbuf.curr_pos()
      break
    }
    92 => {
      _match_pattern = 48
      _match_end = lexbuf.curr_pos()
      break
    }
    93 => {
      _match_pattern = 43
      _match_end = lexbuf.curr_pos()
      break
    }
    94 => {
      _match_pattern = 35
      _match_end = lexbuf.curr_pos()
      break
    }
    95 => {
      _match_pattern = 40
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        46 => 172
        60 => 173
        61 => 174
        _ => break
      }
    }
    96 => {
      _match_pattern = 36
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      _capture_1_start = _match_start + 1
      _capture_1_end = _match_end
      continue match lexbuf.next_as_int() {
        48..=57 => 96
        _ => break
      }
    }
    97 => {
      _match_pattern = 41
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        48..=57 => 97
        65..=90 => 97
        95 => 97
        97..=122 => 97
        194 => 160
        195..=201 => 161
        202 => 162
        225 => 163
        226 => 164
        227 => 165
        228..=233 => 166
        234 => 167
        235..=236 => 166
        237 => 168
        239 => 169
        240 => 170
        243 => 171
        _ => break
      }
    }
    98 => {
      _match_pattern = 42
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _match_end
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
        191 => 159
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
        159 => 157
        160..=174 => 105
        175 => 158
        176..=178 => 105
        _ => break
      }
    }
    110 => {
      continue match lexbuf.next_as_int() {
        160 => 156
        _ => break
      }
    }
    111 => {
      _match_pattern = 30
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 1
      _capture_1_start = _match_start
      _capture_1_end = _match_start + 2
      break
    }
    112 => {
      _match_pattern = 25
      _match_end = lexbuf.curr_pos()
      break
    }
    113 => {
      _match_pattern = 19
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        47 => 175
        48..=57 => 113
        65..=90 => 113
        95 => 113
        97..=122 => 113
        _ => break
      }
    }
    114 => {
      _tag_0 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      _tag_0 = lexbuf.curr_pos()
      _match_pattern = 16
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_1
      _capture_1_start = _tag_1
      _capture_1_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 176
        11..=12 => 176
        14..=45 => 176
        46 => 177
        47 => 176
        48..=57 => 178
        58..=64 => 176
        65..=90 => 178
        91..=94 => 176
        95 => 178
        96 => 176
        97..=122 => 178
        123..=127 => 176
        194..=223 => 179
        224..=239 => 180
        240 => 181
        241..=244 => 182
        _ => break
      }
    }
    115 => {
      _match_pattern = 14
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 2
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 115
        11..=12 => 115
        14..=127 => 115
        194..=223 => 183
        224..=239 => 184
        240 => 185
        241..=244 => 186
        _ => break
      }
    }
    116 => {
      _match_pattern = 13
      _match_end = lexbuf.curr_pos()
      break
    }
    117 => {
      _match_pattern = 12
      _match_end = lexbuf.curr_pos()
      break
    }
    118 => {
      _match_pattern = 24
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 2
      continue match lexbuf.next_as_int() {
        0..=91 => 187
        92 => 188
        93..=127 => 187
        _ => break
      }
    }
    119 => {
      continue match lexbuf.next_as_int() {
        39 => 193
        _ => break
      }
    }
    120 => {
      continue match lexbuf.next_as_int() {
        32 => 189
        34 => 189
        39 => 189
        92 => 189
        98 => 189
        110 => 189
        111 => 190
        114 => 189
        116 => 189
        117 => 191
        120 => 192
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
      _match_pattern = 4
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 125
        11..=12 => 125
        14..=127 => 125
        194..=223 => 194
        224..=239 => 195
        240 => 196
        241..=244 => 197
        _ => break
      }
    }
    126 => {
      _match_pattern = 3
      _match_end = lexbuf.curr_pos()
      break
    }
    127 => {
      _match_pattern = 46
      _match_end = lexbuf.curr_pos()
      break
    }
    128 => {
      _match_pattern = 2
      _match_end = lexbuf.curr_pos()
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
        128 => 198
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
        154 => 200
        _ => break
      }
    }
    134 => {
      continue match lexbuf.next_as_int() {
        128 => 201
        129 => 202
        _ => break
      }
    }
    135 => {
      continue match lexbuf.next_as_int() {
        128 => 200
        _ => break
      }
    }
    136 => {
      continue match lexbuf.next_as_int() {
        187 => 199
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
        159 => 198
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
      _match_pattern = 66
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end + -2
      break
    }
    149 => {
      _match_pattern = 68
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        48..=57 => 149
        69 => 150
        70 => 151
        95 => 149
        101 => 150
        _ => break
      }
    }
    150 => {
      continue match lexbuf.next_as_int() {
        43 => 203
        45 => 203
        48..=57 => 204
        _ => break
      }
    }
    151 => {
      _match_pattern = 67
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    152 => {
      continue match lexbuf.next_as_int() {
        46 => 148
        _ => break
      }
    }
    153 => {
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 152
        48..=49 => 153
        76 => 82
        78 => 82
        85 => 84
        95 => 153
        _ => break
      }
    }
    154 => {
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 152
        48..=55 => 154
        76 => 82
        78 => 82
        85 => 84
        95 => 154
        _ => break
      }
    }
    155 => {
      _match_pattern = 69
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 205
        48..=57 => 155
        65..=70 => 155
        76 => 82
        78 => 82
        85 => 84
        95 => 155
        97..=102 => 155
        _ => break
      }
    }
    156 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 100
        _ => break
      }
    }
    157 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 100
        _ => break
      }
    }
    158 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 100
        _ => break
      }
    }
    159 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 98
        _ => break
      }
    }
    160 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 97
        174..=191 => 97
        _ => break
      }
    }
    161 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 97
        _ => break
      }
    }
    162 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 97
        _ => break
      }
    }
    163 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 161
        184..=187 => 161
        _ => break
      }
    }
    164 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 161
        133..=134 => 161
        186..=187 => 161
        191 => 209
        _ => break
      }
    }
    165 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 161
        135..=191 => 161
        _ => break
      }
    }
    166 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 161
        _ => break
      }
    }
    167 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 161
        _ => break
      }
    }
    168 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 161
        _ => break
      }
    }
    169 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 161
        184..=185 => 161
        _ => break
      }
    }
    170 => {
      continue match lexbuf.next_as_int() {
        159 => 207
        160..=174 => 166
        175 => 208
        176..=178 => 166
        _ => break
      }
    }
    171 => {
      continue match lexbuf.next_as_int() {
        160 => 206
        _ => break
      }
    }
    172 => {
      _match_pattern = 37
      _match_end = lexbuf.curr_pos()
      break
    }
    173 => {
      _match_pattern = 39
      _match_end = lexbuf.curr_pos()
      break
    }
    174 => {
      _match_pattern = 38
      _match_end = lexbuf.curr_pos()
      break
    }
    175 => {
      continue match lexbuf.next_as_int() {
        65..=90 => 113
        95 => 113
        97..=122 => 113
        _ => break
      }
    }
    176 => {
      _match_pattern = 16
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_1
      _capture_1_start = _tag_1
      _capture_1_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 176
        11..=12 => 176
        14..=127 => 176
        194..=223 => 179
        224..=239 => 180
        240 => 181
        241..=244 => 182
        _ => break
      }
    }
    177 => {
      _tag_2 = lexbuf.curr_pos()
      _match_pattern = 16
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_1
      _capture_1_start = _tag_1
      _capture_1_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 176
        11..=12 => 176
        14..=64 => 176
        65..=90 => 210
        91..=94 => 176
        95 => 210
        96 => 176
        97..=122 => 210
        123..=127 => 176
        194..=223 => 179
        224..=239 => 180
        240 => 181
        241..=244 => 182
        _ => break
      }
    }
    178 => {
      _tag_0 = lexbuf.curr_pos()
      _tag_1_1 = _tag_1
      _tag_1 = lexbuf.curr_pos()
      _tag_1_1 = _tag_1
      _tag_1 = lexbuf.curr_pos()
      _tag_0 = lexbuf.curr_pos()
      _match_pattern = 16
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_1
      _capture_1_start = _tag_1
      _capture_1_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 176
        11..=12 => 176
        14..=45 => 176
        46 => 177
        47 => 176
        48..=57 => 178
        58..=64 => 176
        65..=90 => 178
        91..=94 => 176
        95 => 178
        96 => 176
        97..=122 => 178
        123..=127 => 176
        194..=223 => 179
        224..=239 => 180
        240 => 181
        241..=244 => 182
        _ => break
      }
    }
    179 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 176
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
        128..=191 => 115
        _ => break
      }
    }
    184 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 183
        _ => break
      }
    }
    185 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 184
        _ => break
      }
    }
    186 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 184
        _ => break
      }
    }
    187 => {
      continue match lexbuf.next_as_int() {
        39 => 211
        _ => break
      }
    }
    188 => {
      continue match lexbuf.next_as_int() {
        32 => 212
        34 => 212
        39 => 213
        92 => 212
        98 => 212
        110 => 212
        111 => 214
        114 => 212
        116 => 212
        120 => 215
        _ => break
      }
    }
    189 => {
      continue match lexbuf.next_as_int() {
        39 => 220
        _ => break
      }
    }
    190 => {
      continue match lexbuf.next_as_int() {
        48..=51 => 218
        _ => break
      }
    }
    191 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 216
        65..=70 => 216
        97..=102 => 216
        123 => 217
        _ => break
      }
    }
    192 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 219
        65..=70 => 219
        97..=102 => 219
        _ => break
      }
    }
    193 => {
      _match_pattern = 5
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      break
    }
    194 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 125
        _ => break
      }
    }
    195 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 194
        _ => break
      }
    }
    196 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 195
        _ => break
      }
    }
    197 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 195
        _ => break
      }
    }
    198 => {
      _match_pattern = 1
      _match_end = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        9 => 3
        11..=12 => 3
        32 => 3
        48..=57 => 34
        65..=90 => 34
        95 => 34
        97..=122 => 34
        126 => 57
        194 => 221
        195..=201 => 40
        202 => 59
        225 => 222
        226 => 223
        227 => 224
        228..=233 => 47
        234 => 63
        235..=236 => 47
        237 => 64
        239 => 225
        240 => 66
        243 => 67
        _ => break
      }
    }
    199 => {
      continue match lexbuf.next_as_int() {
        191 => 3
        _ => break
      }
    }
    200 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        _ => break
      }
    }
    201 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 3
        175 => 3
        _ => break
      }
    }
    202 => {
      continue match lexbuf.next_as_int() {
        159 => 3
        _ => break
      }
    }
    203 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 204
        _ => break
      }
    }
    204 => {
      _match_pattern = 68
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        48..=57 => 204
        70 => 151
        95 => 204
        _ => break
      }
    }
    205 => {
      _match_pattern = 68
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        46 => 148
        48..=57 => 226
        65..=70 => 226
        80 => 150
        95 => 226
        97..=102 => 226
        112 => 150
        _ => break
      }
    }
    206 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 161
        _ => break
      }
    }
    207 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 161
        _ => break
      }
    }
    208 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 161
        _ => break
      }
    }
    209 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 97
        _ => break
      }
    }
    210 => {
      _tag_3 = lexbuf.curr_pos()
      _match_pattern = 15
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_0
      _capture_1_start = _tag_2
      _capture_1_end = _tag_3
      _capture_2_start = _tag_3
      _capture_2_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 227
        11..=12 => 227
        14..=47 => 227
        48..=57 => 228
        58..=64 => 227
        65..=90 => 228
        91..=94 => 227
        95 => 228
        96 => 227
        97..=122 => 228
        123..=127 => 227
        194..=223 => 229
        224..=239 => 230
        240 => 231
        241..=244 => 232
        _ => break
      }
    }
    211 => {
      _match_pattern = 22
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 2
      _capture_0_end = _match_start + 3
      break
    }
    212 => {
      continue match lexbuf.next_as_int() {
        39 => 233
        _ => break
      }
    }
    213 => {
      _match_pattern = 22
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 2
      _capture_0_end = _match_start + 3
      continue match lexbuf.next_as_int() {
        39 => 233
        _ => break
      }
    }
    214 => {
      continue match lexbuf.next_as_int() {
        48..=51 => 234
        _ => break
      }
    }
    215 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 235
        65..=70 => 235
        97..=102 => 235
        _ => break
      }
    }
    216 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 237
        65..=70 => 237
        97..=102 => 237
        _ => break
      }
    }
    217 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 217
        65..=70 => 217
        97..=102 => 217
        125 => 236
        _ => break
      }
    }
    218 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 238
        _ => break
      }
    }
    219 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 239
        65..=70 => 239
        97..=102 => 239
        _ => break
      }
    }
    220 => {
      _match_pattern = 6
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 4
      break
    }
    221 => {
      continue match lexbuf.next_as_int() {
        160 => 3
        161..=172 => 34
        174..=191 => 34
        _ => break
      }
    }
    222 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 40
        154 => 200
        184..=187 => 40
        _ => break
      }
    }
    223 => {
      continue match lexbuf.next_as_int() {
        128 => 201
        129 => 138
        130 => 40
        133..=134 => 40
        186..=187 => 40
        191 => 143
        _ => break
      }
    }
    224 => {
      continue match lexbuf.next_as_int() {
        128 => 130
        129..=131 => 40
        135..=191 => 40
        _ => break
      }
    }
    225 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 40
        184..=185 => 40
        187 => 199
        _ => break
      }
    }
    226 => {
      _match_pattern = 68
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_end
      continue match lexbuf.next_as_int() {
        48..=57 => 226
        65..=70 => 226
        80 => 150
        95 => 226
        97..=102 => 226
        112 => 150
        _ => break
      }
    }
    227 => {
      _match_pattern = 15
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_0
      _capture_1_start = _tag_2
      _capture_1_end = _tag_3
      _capture_2_start = _tag_3
      _capture_2_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 227
        11..=12 => 227
        14..=127 => 227
        194..=223 => 229
        224..=239 => 230
        240 => 231
        241..=244 => 232
        _ => break
      }
    }
    228 => {
      _tag_3_1 = _tag_3
      _tag_3 = lexbuf.curr_pos()
      _tag_3_1 = _tag_3
      _tag_3 = lexbuf.curr_pos()
      _match_pattern = 15
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 1
      _capture_0_end = _tag_0
      _capture_1_start = _tag_2
      _capture_1_end = _tag_3
      _capture_2_start = _tag_3
      _capture_2_end = _match_end
      continue match lexbuf.next_as_int() {
        0..=9 => 227
        11..=12 => 227
        14..=47 => 227
        48..=57 => 228
        58..=64 => 227
        65..=90 => 228
        91..=94 => 227
        95 => 228
        96 => 227
        97..=122 => 228
        123..=127 => 227
        194..=223 => 229
        224..=239 => 230
        240 => 231
        241..=244 => 232
        _ => break
      }
    }
    229 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 227
        _ => break
      }
    }
    230 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 229
        _ => break
      }
    }
    231 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 230
        _ => break
      }
    }
    232 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 230
        _ => break
      }
    }
    233 => {
      _match_pattern = 23
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 3
      _capture_0_end = _match_start + 4
      break
    }
    234 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 240
        _ => break
      }
    }
    235 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 241
        65..=70 => 241
        97..=102 => 241
        _ => break
      }
    }
    236 => {
      continue match lexbuf.next_as_int() {
        39 => 242
        _ => break
      }
    }
    237 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 243
        65..=70 => 243
        97..=102 => 243
        _ => break
      }
    }
    238 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 244
        _ => break
      }
    }
    239 => {
      continue match lexbuf.next_as_int() {
        39 => 245
        _ => break
      }
    }
    240 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 246
        _ => break
      }
    }
    241 => {
      continue match lexbuf.next_as_int() {
        39 => 247
        _ => break
      }
    }
    242 => {
      _match_pattern = 10
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 4
      _capture_0_end = _match_end + -2
      _capture_1_start = _match_start
      _capture_1_end = _match_end
      break
    }
    243 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 248
        65..=70 => 248
        97..=102 => 248
        _ => break
      }
    }
    244 => {
      continue match lexbuf.next_as_int() {
        39 => 249
        _ => break
      }
    }
    245 => {
      _match_pattern = 7
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 6
      break
    }
    246 => {
      continue match lexbuf.next_as_int() {
        39 => 250
        _ => break
      }
    }
    247 => {
      _match_pattern = 20
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 4
      _capture_0_end = _match_start + 6
      break
    }
    248 => {
      continue match lexbuf.next_as_int() {
        39 => 251
        _ => break
      }
    }
    249 => {
      _match_pattern = 8
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 7
      break
    }
    250 => {
      _match_pattern = 21
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start + 4
      _capture_0_end = _match_start + 7
      break
    }
    251 => {
      _match_pattern = 9
      _match_end = lexbuf.curr_pos()
      _capture_0_start = _match_start
      _capture_0_end = _match_start + 8
      break
    }
    _ => panic()
  }

  guard _match_pattern <= 74 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_match_end)
  match _match_pattern {
    0 => {
      ()

      env.add_token_with_loc(NEWLINE, start=_match_start, end=_match_end)
      env.current_bol = _match_end
      env.current_line += 1
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    1 => {
      ()

      tokens(lexbuf, env~, preserve_comment~)
    
    }
    2 => {
      ()

      env.add_token_with_loc(FAT_ARROW, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    3 => {
      ()

      env.add_token_with_loc(THIN_ARROW, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    4 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_match_start, end = _match_end,InterpInvalidComment)
      }
      if env.comment {
        let comment = Comment::{
          content : repr,
          kind : InlineTrailing,
          consumed_by_docstring : @ref.new(false)
        }
        preserve_comment(comment, _match_start, _match_end)
        env.add_token_with_loc(COMMENT(comment), start=_match_start, end=_match_end)
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    5 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    6 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    7 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    8 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    9 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(CHAR(repr), start=_match_start, end=_match_end)
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
      env.add_token_with_loc(CHAR(repr), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    11 => {
      ()

      let startpos = _match_start
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

      let startpos = _match_start
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
        env.add_lexing_error(start=_match_start, end = _match_end, InterpInvalidMultilineString)
      }
      let startpos = _match_start
      let tok : Token = MULTILINE_INTERP(string(lexbuf, env~, end_with_newline = true, allow_interp = true, startpos~))
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_pos(startpos), env.make_pos(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    14 => {
      ()
      let s = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_match_start, end=_match_end, InterpInvalidMultilineString)
      }
      env.add_token_with_loc(MULTILINE_STRING(s), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    15 => {
      ()
      let ident = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let dot_ident = lexbuf.get_string(_capture_1_start, _capture_1_end)
      let raw_payload = lexbuf.get_string(_capture_2_start, _capture_2_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_match_start, end=_match_end, InterpInvalidAttribute)
      } else {
        env.add_token_with_loc(
          ATTRIBUTE((ident, Some(dot_ident), raw_payload)),
          start=_match_start,
          end=_match_end
        )
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    16 => {
      ()
      let ident = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let raw_payload = lexbuf.get_string(_capture_1_start, _capture_1_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_match_start, end=_match_end, InterpInvalidAttribute)
      } else {
        env.add_token_with_loc(
          ATTRIBUTE((ident, None, raw_payload)),
          start=_match_start,
          end=_match_end
        )
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    17 => {
      ()
      let ident = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let dot_ident = lexbuf.get_string(_capture_1_start, _capture_1_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_match_start, end=_match_end, InterpInvalidAttribute)
      } else {
        env.add_token_with_loc(
          ATTRIBUTE((ident, Some(dot_ident), "")),
          start=_match_start,
          end=_match_end
        )
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    18 => {
      ()
      let ident = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if env.is_interpolation {
        env.add_lexing_error(start=_match_start, end=_match_end, InterpInvalidAttribute)
      } else {
        env.add_token_with_loc(
          ATTRIBUTE((ident, None, "")),
          start=_match_start,
          end=_match_end
        )
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    19 => {
      ()
      let pkgname = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(PACKAGE_NAME(pkgname), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    20 => {
      ()
      let hex = lexbuf.get_string(_capture_0_start, _capture_0_end)

      let literal = "\\x" + hex
      env.add_token_with_loc(BYTE(literal), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    21 => {
      ()
      let oct = lexbuf.get_string(_capture_0_start, _capture_0_end)

      let literal = "\\o" + oct
      env.add_token_with_loc(BYTE(literal), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    22 => {
      ()
      let ascii = lexbuf.get_char(_capture_0_start, _capture_0_end)

      let literal = ascii
      env.add_token_with_loc(BYTE([literal]), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    23 => {
      ()
      let e = lexbuf.get_char(_capture_0_start, _capture_0_end)

      let literal = "\\" + [e]
      env.add_token_with_loc(BYTE(literal), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    24 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = repr

      invalid_byte(lexbuf, env~, start=_capture_0_start)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    25 => {
      ()

      env.add_token_with_loc(AMPERAMPER, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    26 => {
      ()

      env.add_token_with_loc(AMPER, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    27 => {
      ()

      env.add_token_with_loc(CARET, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    28 => {
      ()

      env.add_token_with_loc(LPAREN, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    29 => {
      ()

      env.add_token_with_loc(RPAREN, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    30 => {
      ()
      let op = lexbuf.get_char(_capture_0_start, _capture_0_end)
      let repr = lexbuf.get_string(_capture_1_start, _capture_1_end)
      let _ = repr

      env.add_token_with_loc(AUGMENTED_ASSIGNMENT([op]), start=_capture_1_start, end=_capture_1_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    31 => {
      ()

      env.add_token_with_loc(INFIX3("*"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    32 => {
      ()

      env.add_token_with_loc(INFIX3("/"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    33 => {
      ()

      env.add_token_with_loc(INFIX3("%"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    34 => {
      ()

      env.add_token_with_loc(COMMA, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    35 => {
      ()

      env.add_token_with_loc(DOT_LPAREN, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    36 => {
      ()
      let repr = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = repr
      let digits = lexbuf.get_string(_capture_1_start, _capture_1_end)

      let idx =
        try {
          @strconv.parse_int(digits)
        } catch {
          StrConvError(_) => {
            env.add_lexing_error(InvalidDotInt(repr), start=_capture_0_start, end=_capture_0_end)
            0
          }
        }
      env.add_token_with_loc(DOT_INT(idx), start=_capture_0_start, end=_capture_0_end, start_offset = 1)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    37 => {
      ()

      env.add_token_with_loc(ELLIPSIS, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    38 => {
      ()

      env.add_token_with_loc(RANGE_INCLUSIVE, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    39 => {
      ()

      env.add_token_with_loc(RANGE_EXCLUSIVE, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    40 => {
      ()

      env.add_token_with_loc(DOTDOT, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    41 => {
      ()
      let name = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = name

      env.add_token_with_loc(DOT_UIDENT(name), start=_capture_0_start, end=_capture_0_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    42 => {
      ()
      let name = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = name

      env.add_token_with_loc(DOT_LIDENT(name), start=_capture_0_start, end=_capture_0_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    43 => {
      ()

      env.add_token_with_loc(COLONCOLON, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    44 => {
      ()

      env.add_token_with_loc(COLON, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    45 => {
      ()

      env.add_token_with_loc(SEMI(true), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    46 => {
      ()

      env.add_token_with_loc(INFIX1("=="), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    47 => {
      ()

      env.add_token_with_loc(EQUAL, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    48 => {
      ()

      env.add_token_with_loc(INFIX1("<="), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    49 => {
      ()

      env.add_token_with_loc(INFIX1(">="), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    50 => {
      ()

      env.add_token_with_loc(INFIX2(">>"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    51 => {
      ()

      env.add_token_with_loc(INFIX1(">"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    52 => {
      ()

      env.add_token_with_loc(INFIX2("<<"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    53 => {
      ()

      env.add_token_with_loc(INFIX1("<"), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    54 => {
      ()

      env.add_token_with_loc(LBRACKET, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    55 => {
      ()

      env.add_token_with_loc(RBRACKET, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    56 => {
      ()

      env.add_token_with_loc(LBRACE, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    57 => {
      ()

      env.add_token_with_loc(RBRACE, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    58 => {
      ()

      env.add_token_with_loc(PIPE, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    59 => {
      ()

      env.add_token_with_loc(BARBAR, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    60 => {
      ()

      env.add_token_with_loc(BAR, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    61 => {
      ()

      env.add_token_with_loc(PLUS, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    62 => {
      ()

      env.add_token_with_loc(MINUS, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    63 => {
      ()

      env.add_token_with_loc(QUESTION, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    64 => {
      ()

      env.add_token_with_loc(INFIX1("!="), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    65 => {
      ()

      env.add_token_with_loc(EXCLAMATION, start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    66 => {
      ()
      let integer = lexbuf.get_string(_capture_0_start, _capture_0_end)
      let _ = integer

      lexbuf.reset(pos=_capture_0_end)
      env.add_token_with_loc(INT(integer), start=_capture_0_start, end=_capture_0_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    67 => {
      ()
      let float = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(FLOAT(float), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    68 => {
      ()
      let double = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(DOUBLE(double), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    69 => {
      ()
      let integer = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(INT(integer), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    70 => {
      ()

      let end = lexbuf.curr_pos()
      env.add_token_with_loc(EOF, start=end, end=end)
    
    }
    71 => {
      ()
      let raw = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(UIDENT(raw), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    72 => {
      ()
      let raw = lexbuf.get_string(_capture_0_start, _capture_0_end)

      env.add_token_with_loc(POST_LABEL(raw), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    73 => {
      ()
      let raw = lexbuf.get_string(_capture_0_start, _capture_0_end)

      if reserved_keyword_table.contains(raw) {
        env.add_lexing_error(Reserved_keyword(raw), start=_match_start, end=_match_end)
      }
      env.add_token_with_loc(
        match keyword_table.get(raw) {
          None => LIDENT(raw)
          Some(tok) => tok
        },
        start=_match_start, end=_match_end
      )
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    74 => {
      ()
      let c = lexbuf.get_char(_capture_0_start, _capture_0_end)

      env.add_lexing_error(IllegalCharacter(c), start=_match_start, end=_match_end)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    _ => panic()
  }
}





