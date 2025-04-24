
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
        loc = Loc::{ start : env.make_position(startpos), end : env.make_position(lexbuf.curr_pos()) }
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
  let mut _tag_0 = -1
  let mut _tag_1 = -1
  let mut _tag_2 = -1
  let mut _tag_3 = -1
  let mut _tag_4 = -1
  let mut _tag_5 = -1
  let mut _tag_6 = -1
  let mut _tag_7 = -1
  let mut _tag_8 = -1
  let mut _tag_9 = -1
  let mut _tag_10 = -1
  let mut _tag_11 = -1
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
      _tag_2 = lexbuf.curr_pos()
      _tag_4 = lexbuf.curr_pos()
      _tag_6 = lexbuf.curr_pos()
      _tag_8 = lexbuf.curr_pos()
      _tag_10 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        -1 => 1
        0..=8 => 2
        9 => 3
        10 => 4
        11..=12 => 3
        13 => 5
        14..=31 => 2
        32 => 3
        33 => 2
        34 => 6
        35..=122 => 2
        123 => 6
        124 => 2
        125 => 7
        126..=127 => 2
        194 => 8
        195..=223 => 9
        224 => 10
        225 => 11
        226 => 12
        227 => 13
        228..=236 => 10
        237 => 14
        238 => 10
        239 => 15
        240 => 16
        241..=244 => 17
        _ => break
      }
    }
    1 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_7 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_2
        _capture_0_end = _tag_3
      }
      break
    }
    2 => {
      _tag_9 = lexbuf.curr_pos()
      _tag_11 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_8
        _capture_0_end = _tag_9
      }
      break
    }
    3 => {
      _tag_9 = lexbuf.curr_pos()
      _tag_11 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_8
        _capture_0_end = _tag_9
      }
      continue match lexbuf.next_as_int() {
        9 => 22
        11..=12 => 22
        32 => 22
        125 => 23
        194 => 24
        225 => 25
        226 => 26
        227 => 27
        239 => 28
        _ => break
      }
    }
    4 => {
      _tag_5 = lexbuf.curr_pos()
      _tag_11 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_5
      }
      break
    }
    5 => {
      _tag_5 = lexbuf.curr_pos()
      _tag_9 = lexbuf.curr_pos()
      _tag_11 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_5
      }
      break
    }
    6 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_10
        _capture_0_end = _tag_11
      }
      break
    }
    7 => {
      _tag_1 = lexbuf.curr_pos()
      _tag_9 = lexbuf.curr_pos()
      _tag_11 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    8 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 2
        160 => 3
        161..=191 => 2
        _ => break
      }
    }
    9 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 2
        _ => break
      }
    }
    10 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 9
        _ => break
      }
    }
    11 => {
      continue match lexbuf.next_as_int() {
        128..=153 => 9
        154 => 21
        155..=191 => 9
        _ => break
      }
    }
    12 => {
      continue match lexbuf.next_as_int() {
        128 => 19
        129 => 20
        130..=191 => 9
        _ => break
      }
    }
    13 => {
      continue match lexbuf.next_as_int() {
        128 => 21
        129..=191 => 9
        _ => break
      }
    }
    14 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 9
        160..=191 => 18
        _ => break
      }
    }
    15 => {
      continue match lexbuf.next_as_int() {
        128..=186 => 9
        187 => 29
        188..=191 => 9
        _ => break
      }
    }
    16 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 10
        _ => break
      }
    }
    17 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 10
        _ => break
      }
    }
    18 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 6
        _ => break
      }
    }
    19 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 3
        139..=174 => 2
        175 => 3
        176..=191 => 2
        _ => break
      }
    }
    20 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 2
        159 => 3
        160..=191 => 2
        _ => break
      }
    }
    21 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        129..=191 => 2
        _ => break
      }
    }
    22 => {
      continue match lexbuf.next_as_int() {
        9 => 22
        11..=12 => 22
        32 => 22
        125 => 23
        194 => 24
        225 => 25
        226 => 26
        227 => 27
        239 => 28
        _ => break
      }
    }
    23 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    24 => {
      continue match lexbuf.next_as_int() {
        160 => 22
        _ => break
      }
    }
    25 => {
      continue match lexbuf.next_as_int() {
        154 => 30
        _ => break
      }
    }
    26 => {
      continue match lexbuf.next_as_int() {
        128 => 31
        129 => 32
        _ => break
      }
    }
    27 => {
      continue match lexbuf.next_as_int() {
        128 => 30
        _ => break
      }
    }
    28 => {
      continue match lexbuf.next_as_int() {
        187 => 33
        _ => break
      }
    }
    29 => {
      continue match lexbuf.next_as_int() {
        128..=190 => 2
        191 => 3
        _ => break
      }
    }
    30 => {
      continue match lexbuf.next_as_int() {
        128 => 22
        _ => break
      }
    }
    31 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 22
        175 => 22
        _ => break
      }
    }
    32 => {
      continue match lexbuf.next_as_int() {
        159 => 22
        _ => break
      }
    }
    33 => {
      continue match lexbuf.next_as_int() {
        191 => 22
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
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      _start_pos_of_repr
    
    }
    1 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, UnterminatedString)
      _start_pos_of_repr
    
    }
    2 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, UnterminatedStringInVariableInterploation)
      lexbuf.reset(pos=_start_pos)
      _start_pos_of_repr
    
    }
    3 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, UnterminatedStringInVariableInterploation)
      _start_pos_of_repr
    
    }
    4 => {
      ()
      let _start_pos_of_c = _capture_0_start
      let _end_pos_of_c = _capture_0_end
      let c = lexbuf.get_char(_start_pos_of_c, _end_pos_of_c)

      string_repr_buf.write_char(c)
      interp_handle(lexbuf, env~)
    
    }
    5 => {
      ()
      let _start_pos_of_c = _capture_0_start
      let _end_pos_of_c = _capture_0_end
      let c = lexbuf.get_char(_start_pos_of_c, _end_pos_of_c)

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
  let mut _tag_0 = -1
  let mut _tag_1 = -1
  let mut _tag_2 = -1
  let mut _tag_3 = -1
  let mut _tag_4 = -1
  let mut _tag_5 = -1
  let mut _tag_6 = -1
  let mut _tag_7 = -1
  let mut _tag_8 = -1
  let mut _tag_9 = -1
  let mut _tag_10 = -1
  let mut _tag_11 = -1
  let mut _tag_12 = -1
  let mut _tag_13 = -1
  let mut _tag_14 = -1
  let mut _tag_15 = -1
  let mut _tag_16 = -1
  let mut _tag_17 = -1
  let mut _tag_18 = -1
  let mut _tag_19 = -1
  let mut _tag_20 = -1
  let mut _tag_21 = -1
  let mut _tag_22 = -1
  let mut _tag_23 = -1
  let mut _tag_24 = -1
  let mut _tag_25 = -1
  let mut _tag_26 = -1
  let mut _tag_27 = -1
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
      _tag_2 = lexbuf.curr_pos()
      _tag_4 = lexbuf.curr_pos()
      _tag_6 = lexbuf.curr_pos()
      _tag_8 = lexbuf.curr_pos()
      _tag_10 = lexbuf.curr_pos()
      _tag_12 = lexbuf.curr_pos()
      _tag_14 = lexbuf.curr_pos()
      _tag_16 = lexbuf.curr_pos()
      _tag_18 = lexbuf.curr_pos()
      _tag_20 = lexbuf.curr_pos()
      _tag_22 = lexbuf.curr_pos()
      _tag_24 = lexbuf.curr_pos()
      _tag_26 = lexbuf.curr_pos()
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
      _tag_23 = lexbuf.curr_pos()
      if matched >= 11 {
        matched = 11
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_22
        _capture_0_end = _tag_23
      }
      break
    }
    2 => {
      _tag_27 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_26
        _capture_0_end = _tag_27
      }
      break
    }
    3 => {
      _tag_27 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_26
        _capture_0_end = _tag_27
      }
      continue match lexbuf.next_as_int() {
        10 => 10
        _ => break
      }
    }
    4 => {
      _tag_1 = lexbuf.curr_pos()
      _tag_27 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    5 => {
      _tag_27 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_26
        _capture_0_end = _tag_27
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
      _tag_25 = lexbuf.curr_pos()
      if matched >= 12 {
        matched = 12
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_24
        _capture_0_end = _tag_25
      }
      break
    }
    11 => {
      _tag_21 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_20
        _capture_0_end = _tag_21
      }
      break
    }
    12 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_21 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_2
        _capture_0_end = _tag_3
      }
      break
    }
    13 => {
      _tag_21 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_20
        _capture_0_end = _tag_21
      }
      continue match lexbuf.next_as_int() {
        0..=47 => 29
        48..=51 => 30
        52..=127 => 29
        194..=223 => 31
        224..=239 => 32
        240 => 33
        241..=244 => 34
        _ => break
      }
    }
    14 => {
      _tag_21 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_20
        _capture_0_end = _tag_21
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 21
        65..=70 => 21
        97..=102 => 21
        123 => 22
        _ => break
      }
    }
    15 => {
      _tag_21 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_20
        _capture_0_end = _tag_21
      }
      continue match lexbuf.next_as_int() {
        0..=47 => 35
        48..=57 => 36
        58..=64 => 35
        65..=70 => 36
        71..=96 => 35
        97..=102 => 36
        103..=127 => 35
        194..=223 => 37
        224..=239 => 38
        240 => 39
        241..=244 => 40
        _ => break
      }
    }
    16 => {
      _tag_19 = lexbuf.curr_pos()
      _tag_21 = lexbuf.curr_pos()
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_18
        _capture_0_end = _tag_19
      }
      continue match lexbuf.next_as_int() {
        9 => 23
        11..=12 => 23
        32 => 23
        194 => 24
        225 => 25
        226 => 26
        227 => 27
        239 => 28
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
        48..=57 => 48
        65..=70 => 48
        97..=102 => 48
        _ => break
      }
    }
    22 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 41
        11..=12 => 41
        14..=47 => 41
        48..=57 => 42
        58..=64 => 41
        65..=70 => 42
        71..=96 => 41
        97..=102 => 42
        103..=124 => 41
        125 => 43
        126..=127 => 41
        194..=223 => 44
        224..=239 => 45
        240 => 46
        241..=244 => 47
        _ => break
      }
    }
    23 => {
      _tag_19 = lexbuf.curr_pos()
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_18
        _capture_0_end = _tag_19
      }
      continue match lexbuf.next_as_int() {
        9 => 23
        11..=12 => 23
        32 => 23
        194 => 24
        225 => 25
        226 => 26
        227 => 27
        239 => 28
        _ => break
      }
    }
    24 => {
      continue match lexbuf.next_as_int() {
        160 => 23
        _ => break
      }
    }
    25 => {
      continue match lexbuf.next_as_int() {
        154 => 49
        _ => break
      }
    }
    26 => {
      continue match lexbuf.next_as_int() {
        128 => 51
        129 => 52
        _ => break
      }
    }
    27 => {
      continue match lexbuf.next_as_int() {
        128 => 49
        _ => break
      }
    }
    28 => {
      continue match lexbuf.next_as_int() {
        187 => 50
        _ => break
      }
    }
    29 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 53
        194..=223 => 54
        224..=239 => 55
        240 => 56
        241..=244 => 57
        _ => break
      }
    }
    30 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 53
        48..=55 => 58
        56..=127 => 53
        194..=223 => 54
        224..=239 => 55
        240 => 56
        241..=244 => 57
        _ => break
      }
    }
    31 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 29
        _ => break
      }
    }
    32 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 31
        _ => break
      }
    }
    33 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 32
        _ => break
      }
    }
    34 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 32
        _ => break
      }
    }
    35 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 59
        194..=223 => 60
        224..=239 => 61
        240 => 62
        241..=244 => 63
        _ => break
      }
    }
    36 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 59
        48..=57 => 64
        58..=64 => 59
        65..=70 => 64
        71..=96 => 59
        97..=102 => 64
        103..=127 => 59
        194..=223 => 60
        224..=239 => 61
        240 => 62
        241..=244 => 63
        _ => break
      }
    }
    37 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 35
        _ => break
      }
    }
    38 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 37
        _ => break
      }
    }
    39 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 38
        _ => break
      }
    }
    40 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 38
        _ => break
      }
    }
    41 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 41
        11..=12 => 41
        14..=124 => 41
        125 => 43
        126..=127 => 41
        194..=223 => 44
        224..=239 => 45
        240 => 46
        241..=244 => 47
        _ => break
      }
    }
    42 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 41
        11..=12 => 41
        14..=47 => 41
        48..=57 => 42
        58..=64 => 41
        65..=70 => 42
        71..=96 => 41
        97..=102 => 42
        103..=124 => 41
        125 => 65
        126..=127 => 41
        194..=223 => 44
        224..=239 => 45
        240 => 46
        241..=244 => 47
        _ => break
      }
    }
    43 => {
      _tag_17 = lexbuf.curr_pos()
      if matched >= 8 {
        matched = 8
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_16
        _capture_0_end = _tag_17
      }
      break
    }
    44 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 41
        _ => break
      }
    }
    45 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 44
        _ => break
      }
    }
    46 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 45
        _ => break
      }
    }
    47 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 45
        _ => break
      }
    }
    48 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 66
        65..=70 => 66
        97..=102 => 66
        _ => break
      }
    }
    49 => {
      continue match lexbuf.next_as_int() {
        128 => 23
        _ => break
      }
    }
    50 => {
      continue match lexbuf.next_as_int() {
        191 => 23
        _ => break
      }
    }
    51 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 23
        175 => 23
        _ => break
      }
    }
    52 => {
      continue match lexbuf.next_as_int() {
        159 => 23
        _ => break
      }
    }
    53 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 67
        194..=223 => 68
        224..=239 => 69
        240 => 70
        241..=244 => 71
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
        128..=191 => 54
        _ => break
      }
    }
    56 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 55
        _ => break
      }
    }
    57 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 55
        _ => break
      }
    }
    58 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 67
        48..=55 => 72
        56..=127 => 67
        194..=223 => 68
        224..=239 => 69
        240 => 70
        241..=244 => 71
        _ => break
      }
    }
    59 => {
      _tag_7 = lexbuf.curr_pos()
      if matched >= 3 {
        matched = 3
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_6
        _capture_0_end = _tag_7
      }
      break
    }
    60 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 59
        _ => break
      }
    }
    61 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 60
        _ => break
      }
    }
    62 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 61
        _ => break
      }
    }
    63 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 61
        _ => break
      }
    }
    64 => {
      _tag_5 = lexbuf.curr_pos()
      _tag_7 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_5
      }
      break
    }
    65 => {
      _tag_15 = lexbuf.curr_pos()
      _tag_17 = lexbuf.curr_pos()
      if matched >= 7 {
        matched = 7
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_14
        _capture_0_end = _tag_15
      }
      break
    }
    66 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 73
        65..=70 => 73
        97..=102 => 73
        _ => break
      }
    }
    67 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_10
        _capture_0_end = _tag_11
      }
      break
    }
    68 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 67
        _ => break
      }
    }
    69 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 68
        _ => break
      }
    }
    70 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 69
        _ => break
      }
    }
    71 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 69
        _ => break
      }
    }
    72 => {
      _tag_9 = lexbuf.curr_pos()
      _tag_11 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_8
        _capture_0_end = _tag_9
      }
      break
    }
    73 => {
      _tag_13 = lexbuf.curr_pos()
      if matched >= 6 {
        matched = 6
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_12
        _capture_0_end = _tag_13
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
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if end_with_newline {
        string_repr_buf.write_char('"')
        normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
      } else {
        if not(string_repr_buf.is_empty()) {
          string_interps.val.push(
            InterpLit(
              repr=string_repr_buf.to_string(),
              loc=Loc::{ start : env.make_position(startpos), end : env.make_position(_end_pos_of_repr) }
            )
          )
        }
      }
    
    }
    1 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    2 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    3 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    4 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    5 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    6 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    7 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      string_repr_buf.write_string(repr)
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    8 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr,InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    9 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if allow_interp {
        if not(string_repr_buf.is_empty()) {
          string_interps.val.push(
            InterpLit(
              repr=string_repr_buf.to_string(),
              loc=Loc::{ start : env.make_position(startpos), end : env.make_position(_end_pos_of_repr) }
            )
          )
        }
        string_repr_buf.reset()
        let apos = _end_pos_of_repr
        let bpos = interp_handle(lexbuf, env~)
        let loc = Loc::{ start: env.make_position(apos), end : env.make_position(bpos) }
        if string_repr_buf.is_empty() {
          env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr,InterpMissingExpression)
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
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InvalidEscapeSequence(repr))
      }
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    10 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr,InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    11 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr,UnterminatedString)
      if not(string_repr_buf.is_empty()) {
        string_interps.val.push(
          InterpLit(
            repr=string_repr_buf.to_string(),
            loc=Loc::{ start : env.make_position(startpos), end : env.make_position(_end_pos_of_repr) }
          )
        )
      }
    
    }
    12 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      // we need insert a NEWLINE token here, so back off to main tokenizer
      lexbuf.reset(pos=lexbuf.curr_pos() - 1)
      if not(end_with_newline) {
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr,UnterminatedString)
      }
      if not(string_repr_buf.is_empty()) {
        string_interps.val.push(
          InterpLit(
            repr=string_repr_buf.to_string(),
            loc=Loc::{ start : env.make_position(startpos), end : env.make_position(_end_pos_of_repr) }
          )
        )
      }
    
    }
    13 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)

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
  let mut _tag_0 = -1
  let mut _tag_1 = -1
  let mut _tag_2 = -1
  let mut _tag_3 = -1
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
      _tag_2 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        -1 => 1
        0..=9 => 2
        10 => 3
        11..=12 => 2
        13 => 3
        14..=38 => 2
        39 => 3
        40..=127 => 2
        194..=223 => 4
        224..=239 => 5
        240 => 6
        241..=244 => 7
        _ => break
      }
    }
    1 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    2 => {
      _tag_3 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_2
        _capture_0_end = _tag_3
      }
      break
    }
    3 => {
      _tag_1 = lexbuf.curr_pos()
      _tag_3 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    4 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 2
        _ => break
      }
    }
    5 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 4
        _ => break
      }
    }
    6 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 5
        _ => break
      }
    }
    7 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 5
        _ => break
      }
    }
    _ => panic()
  }

  guard matched <= 1 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_end_pos)
  match matched {
    0 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_lexing_error(InvalidByteLiteral(invalid_byte_repr_buf.to_string()), start~, end=_end_pos_of_repr)
      invalid_byte_repr_buf.reset()
    
    }
    1 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)

      guard repr is c
      // if is_common_ascii(c) || is_valid_unicode_codepoint(c.to_uint()) {
        invalid_byte_repr_buf.write_char(c)
      // }
      invalid_byte(lexbuf, env~, start~)
    
    }
    _ => panic()
  }
}


fn tokens(lexbuf : Lexbuf, env~ : LexEnv, preserve_comment~ : (Comment, Int, Int) -> Unit) -> Array[TokenTriple]  {
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
  let mut _tag_0 = -1
  let mut _tag_1 = -1
  let mut _tag_2 = -1
  let mut _tag_3 = -1
  let mut _tag_4 = -1
  let mut _tag_5 = -1
  let mut _tag_6 = -1
  let mut _tag_7 = -1
  let mut _tag_8 = -1
  let mut _tag_9 = -1
  let mut _tag_10 = -1
  let mut _tag_11 = -1
  let mut _tag_12 = -1
  let mut _tag_13 = -1
  let mut _tag_14 = -1
  let mut _tag_15 = -1
  let mut _tag_16 = -1
  let mut _tag_17 = -1
  let mut _tag_18 = -1
  let mut _tag_19 = -1
  let mut _tag_20 = -1
  let mut _tag_21 = -1
  let mut _tag_22 = -1
  let mut _tag_23 = -1
  let mut _tag_24 = -1
  let mut _tag_25 = -1
  let mut _tag_26 = -1
  let mut _tag_27 = -1
  let mut _tag_28 = -1
  let mut _tag_29 = -1
  let mut _tag_30 = -1
  let mut _tag_31 = -1
  let mut _tag_32 = -1
  let mut _tag_33 = -1
  let mut _tag_34 = -1
  let mut _tag_35 = -1
  let mut _tag_36 = -1
  let mut _tag_37 = -1
  let mut _tag_38 = -1
  let mut _tag_39 = -1
  let mut _tag_40 = -1
  let mut _tag_41 = -1
  let mut _tag_42 = -1
  let mut _tag_43 = -1
  let mut _tag_44 = -1
  let mut _tag_45 = -1
  let mut _tag_46 = -1
  let mut _tag_47 = -1
  let mut _tag_48 = -1
  let mut _tag_49 = -1
  let mut _tag_50 = -1
  let mut _tag_51 = -1
  let mut _tag_52 = -1
  let mut _tag_53 = -1
  let mut _tag_54 = -1
  let mut _tag_55 = -1
  let mut _tag_56 = -1
  let mut _tag_57 = -1
  let mut _tag_58 = -1
  let mut _tag_59 = -1
  let mut _tag_60 = -1
  let mut _tag_61 = -1
  let mut _tag_62 = -1
  let mut _tag_63 = -1
  let mut _tag_64 = -1
  let mut _tag_65 = -1
  let mut _tag_66 = -1
  let mut _tag_67 = -1
  let mut _tag_68 = -1
  let mut _tag_69 = -1
  let mut _tag_70 = -1
  let mut _tag_71 = -1
  let mut _tag_72 = -1
  let mut _tag_73 = -1
  let mut _tag_74 = -1
  let mut _tag_75 = -1
  let mut _tag_76 = -1
  let mut _tag_77 = -1
  let mut _tag_78 = -1
  let mut _tag_79 = -1
  let mut _tag_80 = -1
  let mut _tag_81 = -1
  let mut _tag_82 = -1
  let mut _tag_83 = -1
  let mut _tag_84 = -1
  let mut _tag_85 = -1
  let mut _tag_86 = -1
  let mut _tag_87 = -1
  let mut _tag_88 = -1
  let mut _tag_89 = -1
  let mut _tag_90 = -1
  let mut _tag_91 = -1
  let mut _tag_92 = -1
  let mut _tag_93 = -1
  let mut _tag_94 = -1
  let mut _tag_95 = -1
  let mut _tag_96 = -1
  let mut _tag_97 = -1
  let mut _tag_98 = -1
  let mut _tag_99 = -1
  let mut _tag_100 = -1
  let mut _tag_101 = -1
  let mut _tag_102 = -1
  let mut _tag_103 = -1
  let mut _tag_104 = -1
  let mut _tag_105 = -1
  let mut _tag_106 = -1
  let mut _tag_107 = -1
  let mut _tag_108 = -1
  let mut _tag_109 = -1
  let mut _tag_110 = -1
  let mut _tag_111 = -1
  let mut _tag_112 = -1
  let mut _tag_113 = -1
  let mut _tag_114 = -1
  let mut _tag_115 = -1
  let mut _tag_116 = -1
  let mut _tag_117 = -1
  let mut _tag_118 = -1
  let mut _tag_119 = -1
  let mut _tag_120 = -1
  let mut _tag_121 = -1
  let mut _tag_122 = -1
  let mut _tag_123 = -1
  let mut _tag_124 = -1
  let mut _tag_125 = -1
  let mut _tag_126 = -1
  let mut _tag_127 = -1
  let mut _tag_128 = -1
  let mut _tag_129 = -1
  let mut _tag_130 = -1
  let mut _tag_131 = -1
  let mut _tag_132 = -1
  let mut _tag_133 = -1
  let mut _tag_134 = -1
  let mut _tag_135 = -1
  let mut _tag_136 = -1
  let mut _tag_137 = -1
  let mut _tag_138 = -1
  let mut _tag_139 = -1
  let mut _tag_140 = -1
  let mut _tag_141 = -1
  let mut _tag_142 = -1
  let mut _tag_143 = -1
  let mut _tag_144 = -1
  let mut _tag_145 = -1
  let mut _tag_146 = -1
  let mut _tag_147 = -1
  let mut _tag_148 = -1
  let mut _tag_149 = -1
  let mut _tag_150 = -1
  let mut _tag_151 = -1
  let mut _tag_152 = -1
  let mut _tag_153 = -1
  let mut _tag_154 = -1
  let mut _tag_155 = -1
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
      _tag_2 = lexbuf.curr_pos()
      _tag_4 = lexbuf.curr_pos()
      _tag_6 = lexbuf.curr_pos()
      _tag_8 = lexbuf.curr_pos()
      _tag_10 = lexbuf.curr_pos()
      _tag_12 = lexbuf.curr_pos()
      _tag_14 = lexbuf.curr_pos()
      _tag_16 = lexbuf.curr_pos()
      _tag_20 = lexbuf.curr_pos()
      _tag_22 = lexbuf.curr_pos()
      _tag_24 = lexbuf.curr_pos()
      _tag_26 = lexbuf.curr_pos()
      _tag_30 = lexbuf.curr_pos()
      _tag_34 = lexbuf.curr_pos()
      _tag_38 = lexbuf.curr_pos()
      _tag_42 = lexbuf.curr_pos()
      _tag_46 = lexbuf.curr_pos()
      _tag_50 = lexbuf.curr_pos()
      _tag_54 = lexbuf.curr_pos()
      _tag_56 = lexbuf.curr_pos()
      _tag_58 = lexbuf.curr_pos()
      _tag_60 = lexbuf.curr_pos()
      _tag_62 = lexbuf.curr_pos()
      _tag_64 = lexbuf.curr_pos()
      _tag_66 = lexbuf.curr_pos()
      _tag_68 = lexbuf.curr_pos()
      _tag_70 = lexbuf.curr_pos()
      _tag_72 = lexbuf.curr_pos()
      _tag_74 = lexbuf.curr_pos()
      _tag_76 = lexbuf.curr_pos()
      _tag_78 = lexbuf.curr_pos()
      _tag_80 = lexbuf.curr_pos()
      _tag_84 = lexbuf.curr_pos()
      _tag_86 = lexbuf.curr_pos()
      _tag_88 = lexbuf.curr_pos()
      _tag_90 = lexbuf.curr_pos()
      _tag_92 = lexbuf.curr_pos()
      _tag_98 = lexbuf.curr_pos()
      _tag_100 = lexbuf.curr_pos()
      _tag_102 = lexbuf.curr_pos()
      _tag_104 = lexbuf.curr_pos()
      _tag_106 = lexbuf.curr_pos()
      _tag_108 = lexbuf.curr_pos()
      _tag_110 = lexbuf.curr_pos()
      _tag_112 = lexbuf.curr_pos()
      _tag_114 = lexbuf.curr_pos()
      _tag_116 = lexbuf.curr_pos()
      _tag_118 = lexbuf.curr_pos()
      _tag_120 = lexbuf.curr_pos()
      _tag_122 = lexbuf.curr_pos()
      _tag_124 = lexbuf.curr_pos()
      _tag_126 = lexbuf.curr_pos()
      _tag_128 = lexbuf.curr_pos()
      _tag_130 = lexbuf.curr_pos()
      _tag_132 = lexbuf.curr_pos()
      _tag_134 = lexbuf.curr_pos()
      _tag_136 = lexbuf.curr_pos()
      _tag_138 = lexbuf.curr_pos()
      _tag_140 = lexbuf.curr_pos()
      _tag_142 = lexbuf.curr_pos()
      _tag_144 = lexbuf.curr_pos()
      _tag_146 = lexbuf.curr_pos()
      _tag_148 = lexbuf.curr_pos()
      _tag_150 = lexbuf.curr_pos()
      _tag_152 = lexbuf.curr_pos()
      _tag_154 = lexbuf.curr_pos()
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
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    2 => {
      _tag_155 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_154
        _capture_0_end = _tag_155
      }
      break
    }
    3 => {
      _tag_155 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 80
        11..=12 => 80
        32 => 80
        194 => 81
        225 => 82
        226 => 83
        227 => 84
        239 => 85
        _ => break
      }
    }
    4 => {
      _tag_1 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    5 => {
      _tag_1 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        10 => 92
        _ => break
      }
    }
    6 => {
      _tag_141 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 61 {
        matched = 61
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_140
        _capture_0_end = _tag_141
      }
      continue match lexbuf.next_as_int() {
        61 => 89
        _ => break
      }
    }
    7 => {
      _tag_23 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 11 {
        matched = 11
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_22
        _capture_0_end = _tag_23
      }
      break
    }
    8 => {
      _tag_32 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_154
        _capture_0_end = _tag_155
      }
      continue match lexbuf.next_as_int() {
        65..=90 => 144
        97..=122 => 144
        124 => 145
        _ => break
      }
    }
    9 => {
      _tag_155 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_154
        _capture_0_end = _tag_155
      }
      continue match lexbuf.next_as_int() {
        124 => 108
        _ => break
      }
    }
    10 => {
      _tag_69 = lexbuf.curr_pos()
      _tag_77 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 30 {
        matched = 30
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_76
        _capture_0_end = _tag_77
      }
      continue match lexbuf.next_as_int() {
        61 => 69
        _ => break
      }
    }
    11 => {
      _tag_61 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 23 {
        matched = 23
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_60
        _capture_0_end = _tag_61
      }
      continue match lexbuf.next_as_int() {
        38 => 86
        _ => break
      }
    }
    12 => {
      _tag_155 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_154
        _capture_0_end = _tag_155
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 63
        11..=12 => 63
        14..=38 => 63
        40..=91 => 63
        92 => 64
        93..=127 => 63
        194..=223 => 65
        224..=239 => 66
        240 => 67
        241..=244 => 68
        _ => break
      }
    }
    13 => {
      _tag_65 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 25 {
        matched = 25
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_64
        _capture_0_end = _tag_65
      }
      break
    }
    14 => {
      _tag_67 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 26 {
        matched = 26
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_66
        _capture_0_end = _tag_67
      }
      break
    }
    15 => {
      _tag_69 = lexbuf.curr_pos()
      _tag_73 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 28 {
        matched = 28
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_72
        _capture_0_end = _tag_73
      }
      continue match lexbuf.next_as_int() {
        61 => 69
        _ => break
      }
    }
    16 => {
      _tag_69 = lexbuf.curr_pos()
      _tag_133 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 57 {
        matched = 57
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_132
        _capture_0_end = _tag_133
      }
      continue match lexbuf.next_as_int() {
        61 => 69
        _ => break
      }
    }
    17 => {
      _tag_79 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 31 {
        matched = 31
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_78
        _capture_0_end = _tag_79
      }
      break
    }
    18 => {
      _tag_69 = lexbuf.curr_pos()
      _tag_135 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 58 {
        matched = 58
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_134
        _capture_0_end = _tag_135
      }
      continue match lexbuf.next_as_int() {
        61 => 69
        62 => 70
        _ => break
      }
    }
    19 => {
      _tag_82 = lexbuf.curr_pos()
      _tag_94 = lexbuf.curr_pos()
      _tag_96 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_154
        _capture_0_end = _tag_155
      }
      continue match lexbuf.next_as_int() {
        40 => 127
        46 => 128
        48..=57 => 129
        65..=90 => 130
        95 => 131
        97..=122 => 131
        194 => 132
        195..=201 => 133
        202 => 134
        225 => 135
        226 => 136
        227 => 137
        228..=233 => 138
        234 => 139
        235..=236 => 138
        237 => 140
        239 => 141
        240 => 142
        243 => 143
        _ => break
      }
    }
    20 => {
      _tag_69 = lexbuf.curr_pos()
      _tag_75 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 29 {
        matched = 29
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_74
        _capture_0_end = _tag_75
      }
      continue match lexbuf.next_as_int() {
        47 => 90
        61 => 69
        _ => break
      }
    }
    21 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 73
        48..=57 => 74
        66 => 75
        76 => 76
        78 => 76
        79 => 77
        85 => 78
        88 => 79
        95 => 74
        98 => 75
        111 => 77
        120 => 79
        _ => break
      }
    }
    22 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 73
        48..=57 => 74
        76 => 76
        78 => 76
        85 => 78
        95 => 74
        _ => break
      }
    }
    23 => {
      _tag_101 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 41 {
        matched = 41
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_100
        _capture_0_end = _tag_101
      }
      continue match lexbuf.next_as_int() {
        58 => 91
        _ => break
      }
    }
    24 => {
      _tag_103 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 42 {
        matched = 42
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_102
        _capture_0_end = _tag_103
      }
      break
    }
    25 => {
      _tag_117 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 49 {
        matched = 49
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_116
        _capture_0_end = _tag_117
      }
      continue match lexbuf.next_as_int() {
        60 => 126
        61 => 87
        _ => break
      }
    }
    26 => {
      _tag_107 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 44 {
        matched = 44
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_106
        _capture_0_end = _tag_107
      }
      continue match lexbuf.next_as_int() {
        61 => 71
        62 => 72
        _ => break
      }
    }
    27 => {
      _tag_113 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 47 {
        matched = 47
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_112
        _capture_0_end = _tag_113
      }
      continue match lexbuf.next_as_int() {
        61 => 87
        62 => 88
        _ => break
      }
    }
    28 => {
      _tag_137 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 59 {
        matched = 59
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_136
        _capture_0_end = _tag_137
      }
      break
    }
    29 => {
      _tag_36 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_154
        _capture_0_end = _tag_155
      }
      continue match lexbuf.next_as_int() {
        65..=90 => 93
        95 => 93
        97..=122 => 93
        _ => break
      }
    }
    30 => {
      _tag_149 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 66 {
        matched = 66
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_148
        _capture_0_end = _tag_149
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 111
        65..=90 => 111
        95 => 111
        97..=122 => 111
        194 => 112
        195..=201 => 113
        202 => 114
        225 => 115
        226 => 116
        227 => 117
        228..=233 => 118
        234 => 119
        235..=236 => 118
        237 => 120
        239 => 121
        240 => 122
        243 => 123
        _ => break
      }
    }
    31 => {
      _tag_119 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 50 {
        matched = 50
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_118
        _capture_0_end = _tag_119
      }
      break
    }
    32 => {
      _tag_121 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 51 {
        matched = 51
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_120
        _capture_0_end = _tag_121
      }
      break
    }
    33 => {
      _tag_63 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 24 {
        matched = 24
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_62
        _capture_0_end = _tag_63
      }
      break
    }
    34 => {
      _tag_151 = lexbuf.curr_pos()
      _tag_153 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 68 {
        matched = 68
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_152
        _capture_0_end = _tag_153
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 94
        65..=90 => 94
        95 => 94
        97..=122 => 94
        126 => 95
        194 => 96
        195..=201 => 97
        202 => 98
        225 => 99
        226 => 100
        227 => 101
        228..=233 => 102
        234 => 103
        235..=236 => 102
        237 => 104
        239 => 105
        240 => 106
        243 => 107
        _ => break
      }
    }
    35 => {
      _tag_151 = lexbuf.curr_pos()
      _tag_153 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 68 {
        matched = 68
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_152
        _capture_0_end = _tag_153
      }
      continue match lexbuf.next_as_int() {
        34 => 124
        39 => 125
        48..=57 => 94
        65..=90 => 94
        95 => 94
        97..=122 => 94
        126 => 95
        194 => 96
        195..=201 => 97
        202 => 98
        225 => 99
        226 => 100
        227 => 101
        228..=233 => 102
        234 => 103
        235..=236 => 102
        237 => 104
        239 => 105
        240 => 106
        243 => 107
        _ => break
      }
    }
    36 => {
      _tag_123 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 52 {
        matched = 52
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_122
        _capture_0_end = _tag_123
      }
      break
    }
    37 => {
      _tag_131 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 56 {
        matched = 56
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_130
        _capture_0_end = _tag_131
      }
      continue match lexbuf.next_as_int() {
        62 => 109
        124 => 110
        _ => break
      }
    }
    38 => {
      _tag_125 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 53 {
        matched = 53
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_124
        _capture_0_end = _tag_125
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
        154 => 61
        155..=183 => 42
        184..=187 => 40
        188..=191 => 42
        _ => break
      }
    }
    45 => {
      continue match lexbuf.next_as_int() {
        128 => 58
        129 => 59
        130 => 40
        131..=132 => 42
        133..=134 => 40
        135..=185 => 42
        186..=187 => 40
        188..=190 => 42
        191 => 60
        _ => break
      }
    }
    46 => {
      continue match lexbuf.next_as_int() {
        128 => 62
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
        187 => 57
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
      continue match lexbuf.next_as_int() {
        128..=190 => 2
        191 => 3
        _ => break
      }
    }
    58 => {
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
    59 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 34
        159 => 146
        160..=191 => 34
        _ => break
      }
    }
    60 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 2
        176..=191 => 34
        _ => break
      }
    }
    61 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        129..=191 => 2
        _ => break
      }
    }
    62 => {
      continue match lexbuf.next_as_int() {
        128 => 146
        129..=191 => 34
        _ => break
      }
    }
    63 => {
      continue match lexbuf.next_as_int() {
        39 => 147
        _ => break
      }
    }
    64 => {
      continue match lexbuf.next_as_int() {
        32 => 148
        34 => 148
        39 => 148
        92 => 148
        98 => 148
        110 => 148
        111 => 149
        114 => 148
        116 => 148
        117 => 150
        120 => 151
        _ => break
      }
    }
    65 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 63
        _ => break
      }
    }
    66 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 65
        _ => break
      }
    }
    67 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 66
        _ => break
      }
    }
    68 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 66
        _ => break
      }
    }
    69 => {
      _tag_71 = lexbuf.curr_pos()
      if matched >= 27 {
        matched = 27
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_68
        _capture_0_end = _tag_69
        _capture_1_start = _tag_70
        _capture_1_end = _tag_71
      }
      break
    }
    70 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 3 {
        matched = 3
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_5
      }
      break
    }
    71 => {
      _tag_105 = lexbuf.curr_pos()
      if matched >= 43 {
        matched = 43
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_104
        _capture_0_end = _tag_105
      }
      break
    }
    72 => {
      _tag_3 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_2
        _capture_0_end = _tag_3
      }
      break
    }
    73 => {
      _tag_145 = lexbuf.curr_pos()
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_144
        _capture_0_end = _tag_145
      }
      continue match lexbuf.next_as_int() {
        46 => 156
        48..=57 => 157
        69 => 158
        95 => 157
        101 => 158
        _ => break
      }
    }
    74 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 73
        48..=57 => 74
        76 => 76
        78 => 76
        85 => 78
        95 => 74
        _ => break
      }
    }
    75 => {
      continue match lexbuf.next_as_int() {
        48..=49 => 153
        _ => break
      }
    }
    76 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 154
        _ => break
      }
    }
    77 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 155
        _ => break
      }
    }
    78 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 154
        76 => 76
        _ => break
      }
    }
    79 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 152
        65..=70 => 152
        97..=102 => 152
        _ => break
      }
    }
    80 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 80
        11..=12 => 80
        32 => 80
        194 => 81
        225 => 82
        226 => 83
        227 => 84
        239 => 85
        _ => break
      }
    }
    81 => {
      continue match lexbuf.next_as_int() {
        160 => 80
        _ => break
      }
    }
    82 => {
      continue match lexbuf.next_as_int() {
        154 => 159
        _ => break
      }
    }
    83 => {
      continue match lexbuf.next_as_int() {
        128 => 161
        129 => 162
        _ => break
      }
    }
    84 => {
      continue match lexbuf.next_as_int() {
        128 => 159
        _ => break
      }
    }
    85 => {
      continue match lexbuf.next_as_int() {
        187 => 160
        _ => break
      }
    }
    86 => {
      _tag_59 = lexbuf.curr_pos()
      if matched >= 22 {
        matched = 22
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_58
        _capture_0_end = _tag_59
      }
      break
    }
    87 => {
      _tag_109 = lexbuf.curr_pos()
      if matched >= 45 {
        matched = 45
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_108
        _capture_0_end = _tag_109
      }
      break
    }
    88 => {
      _tag_111 = lexbuf.curr_pos()
      if matched >= 46 {
        matched = 46
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_110
        _capture_0_end = _tag_111
      }
      break
    }
    89 => {
      _tag_139 = lexbuf.curr_pos()
      if matched >= 60 {
        matched = 60
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_138
        _capture_0_end = _tag_139
      }
      break
    }
    90 => {
      _tag_7 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_6
        _capture_0_end = _tag_7
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 90
        11..=12 => 90
        14..=127 => 90
        194..=223 => 163
        224..=239 => 164
        240 => 165
        241..=244 => 166
        _ => break
      }
    }
    91 => {
      _tag_99 = lexbuf.curr_pos()
      if matched >= 40 {
        matched = 40
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_98
        _capture_0_end = _tag_99
      }
      break
    }
    92 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    93 => {
      _tag_37 = lexbuf.curr_pos()
      _tag_39 = lexbuf.curr_pos()
      if matched >= 16 {
        matched = 16
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_36
        _capture_0_end = _tag_37
        _capture_1_start = _tag_38
        _capture_1_end = _tag_39
      }
      continue match lexbuf.next_as_int() {
        47 => 167
        48..=57 => 93
        65..=90 => 93
        95 => 93
        97..=122 => 93
        _ => break
      }
    }
    94 => {
      _tag_151 = lexbuf.curr_pos()
      _tag_153 = lexbuf.curr_pos()
      if matched >= 68 {
        matched = 68
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_152
        _capture_0_end = _tag_153
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 94
        65..=90 => 94
        95 => 94
        97..=122 => 94
        126 => 95
        194 => 96
        195..=201 => 97
        202 => 98
        225 => 99
        226 => 100
        227 => 101
        228..=233 => 102
        234 => 103
        235..=236 => 102
        237 => 104
        239 => 105
        240 => 106
        243 => 107
        _ => break
      }
    }
    95 => {
      if matched >= 67 {
        matched = 67
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_150
        _capture_0_end = _tag_151
      }
      break
    }
    96 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 94
        174..=191 => 94
        _ => break
      }
    }
    97 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 94
        _ => break
      }
    }
    98 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 94
        _ => break
      }
    }
    99 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 97
        184..=187 => 97
        _ => break
      }
    }
    100 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 97
        133..=134 => 97
        186..=187 => 97
        191 => 171
        _ => break
      }
    }
    101 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 97
        135..=191 => 97
        _ => break
      }
    }
    102 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 97
        _ => break
      }
    }
    103 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 97
        _ => break
      }
    }
    104 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 97
        _ => break
      }
    }
    105 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 97
        184..=185 => 97
        _ => break
      }
    }
    106 => {
      continue match lexbuf.next_as_int() {
        159 => 169
        160..=174 => 102
        175 => 170
        176..=178 => 102
        _ => break
      }
    }
    107 => {
      continue match lexbuf.next_as_int() {
        160 => 168
        _ => break
      }
    }
    108 => {
      _tag_27 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_26
        _capture_0_end = _tag_27
      }
      break
    }
    109 => {
      _tag_127 = lexbuf.curr_pos()
      if matched >= 54 {
        matched = 54
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_126
        _capture_0_end = _tag_127
      }
      break
    }
    110 => {
      _tag_129 = lexbuf.curr_pos()
      if matched >= 55 {
        matched = 55
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_128
        _capture_0_end = _tag_129
      }
      break
    }
    111 => {
      _tag_149 = lexbuf.curr_pos()
      if matched >= 66 {
        matched = 66
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_148
        _capture_0_end = _tag_149
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 111
        65..=90 => 111
        95 => 111
        97..=122 => 111
        194 => 112
        195..=201 => 113
        202 => 114
        225 => 115
        226 => 116
        227 => 117
        228..=233 => 118
        234 => 119
        235..=236 => 118
        237 => 120
        239 => 121
        240 => 122
        243 => 123
        _ => break
      }
    }
    112 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 111
        174..=191 => 111
        _ => break
      }
    }
    113 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 111
        _ => break
      }
    }
    114 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 111
        _ => break
      }
    }
    115 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 113
        184..=187 => 113
        _ => break
      }
    }
    116 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 113
        133..=134 => 113
        186..=187 => 113
        191 => 175
        _ => break
      }
    }
    117 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 113
        135..=191 => 113
        _ => break
      }
    }
    118 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 113
        _ => break
      }
    }
    119 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 113
        _ => break
      }
    }
    120 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 113
        _ => break
      }
    }
    121 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 113
        184..=185 => 113
        _ => break
      }
    }
    122 => {
      continue match lexbuf.next_as_int() {
        159 => 173
        160..=174 => 118
        175 => 174
        176..=178 => 118
        _ => break
      }
    }
    123 => {
      continue match lexbuf.next_as_int() {
        160 => 172
        _ => break
      }
    }
    124 => {
      _tag_25 = lexbuf.curr_pos()
      if matched >= 12 {
        matched = 12
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_24
        _capture_0_end = _tag_25
      }
      break
    }
    125 => {
      _tag_48 = lexbuf.curr_pos()
      _tag_57 = lexbuf.curr_pos()
      if matched >= 21 {
        matched = 21
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_56
        _capture_0_end = _tag_57
      }
      continue match lexbuf.next_as_int() {
        0..=91 => 176
        92 => 177
        93..=127 => 176
        _ => break
      }
    }
    126 => {
      _tag_115 = lexbuf.curr_pos()
      if matched >= 48 {
        matched = 48
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_114
        _capture_0_end = _tag_115
      }
      break
    }
    127 => {
      _tag_81 = lexbuf.curr_pos()
      if matched >= 32 {
        matched = 32
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_80
        _capture_0_end = _tag_81
      }
      break
    }
    128 => {
      _tag_93 = lexbuf.curr_pos()
      if matched >= 37 {
        matched = 37
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_92
        _capture_0_end = _tag_93
      }
      continue match lexbuf.next_as_int() {
        46 => 194
        60 => 195
        61 => 196
        _ => break
      }
    }
    129 => {
      _tag_83 = lexbuf.curr_pos()
      _tag_85 = lexbuf.curr_pos()
      if matched >= 33 {
        matched = 33
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_82
        _capture_0_end = _tag_83
        _capture_1_start = _tag_84
        _capture_1_end = _tag_85
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 129
        _ => break
      }
    }
    130 => {
      _tag_95 = lexbuf.curr_pos()
      if matched >= 38 {
        matched = 38
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_94
        _capture_0_end = _tag_95
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 130
        65..=90 => 130
        95 => 130
        97..=122 => 130
        194 => 178
        195..=201 => 179
        202 => 180
        225 => 181
        226 => 182
        227 => 183
        228..=233 => 184
        234 => 185
        235..=236 => 184
        237 => 186
        239 => 187
        240 => 188
        243 => 189
        _ => break
      }
    }
    131 => {
      _tag_97 = lexbuf.curr_pos()
      if matched >= 39 {
        matched = 39
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_96
        _capture_0_end = _tag_97
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 131
        65..=90 => 131
        95 => 131
        97..=122 => 131
        194 => 132
        195..=201 => 133
        202 => 134
        225 => 135
        226 => 136
        227 => 137
        228..=233 => 138
        234 => 139
        235..=236 => 138
        237 => 140
        239 => 141
        240 => 142
        243 => 143
        _ => break
      }
    }
    132 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 131
        174..=191 => 131
        _ => break
      }
    }
    133 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 131
        _ => break
      }
    }
    134 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 131
        _ => break
      }
    }
    135 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 133
        184..=187 => 133
        _ => break
      }
    }
    136 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 133
        133..=134 => 133
        186..=187 => 133
        191 => 193
        _ => break
      }
    }
    137 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 133
        135..=191 => 133
        _ => break
      }
    }
    138 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 133
        _ => break
      }
    }
    139 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 133
        _ => break
      }
    }
    140 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 133
        _ => break
      }
    }
    141 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 133
        184..=185 => 133
        _ => break
      }
    }
    142 => {
      continue match lexbuf.next_as_int() {
        159 => 191
        160..=174 => 138
        175 => 192
        176..=178 => 138
        _ => break
      }
    }
    143 => {
      continue match lexbuf.next_as_int() {
        160 => 190
        _ => break
      }
    }
    144 => {
      _tag_33 = lexbuf.curr_pos()
      _tag_35 = lexbuf.curr_pos()
      if matched >= 15 {
        matched = 15
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_32
        _capture_0_end = _tag_33
        _capture_1_start = _tag_34
        _capture_1_end = _tag_35
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 144
        11..=12 => 144
        14..=127 => 144
        194..=223 => 197
        224..=239 => 198
        240 => 199
        241..=244 => 200
        _ => break
      }
    }
    145 => {
      _tag_28 = lexbuf.curr_pos()
      _tag_29 = lexbuf.curr_pos()
      _tag_31 = lexbuf.curr_pos()
      if matched >= 14 {
        matched = 14
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_28
        _capture_0_end = _tag_29
        _capture_1_start = _tag_30
        _capture_1_end = _tag_31
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 201
        11..=12 => 201
        14..=127 => 201
        194..=223 => 202
        224..=239 => 203
        240 => 204
        241..=244 => 205
        _ => break
      }
    }
    146 => {
      _tag_151 = lexbuf.curr_pos()
      _tag_153 = lexbuf.curr_pos()
      _tag_155 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 80
        11..=12 => 80
        32 => 80
        48..=57 => 94
        65..=90 => 94
        95 => 94
        97..=122 => 94
        126 => 95
        194 => 206
        195..=201 => 97
        202 => 98
        225 => 207
        226 => 208
        227 => 209
        228..=233 => 102
        234 => 103
        235..=236 => 102
        237 => 104
        239 => 210
        240 => 106
        243 => 107
        _ => break
      }
    }
    147 => {
      _tag_9 = lexbuf.curr_pos()
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_8
        _capture_0_end = _tag_9
      }
      break
    }
    148 => {
      continue match lexbuf.next_as_int() {
        39 => 213
        _ => break
      }
    }
    149 => {
      continue match lexbuf.next_as_int() {
        48..=51 => 214
        _ => break
      }
    }
    150 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 211
        65..=70 => 211
        97..=102 => 211
        123 => 212
        _ => break
      }
    }
    151 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 215
        65..=70 => 215
        97..=102 => 215
        _ => break
      }
    }
    152 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 216
        48..=57 => 152
        65..=70 => 152
        76 => 76
        78 => 76
        85 => 78
        95 => 152
        97..=102 => 152
        _ => break
      }
    }
    153 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 154
        48..=49 => 153
        76 => 76
        78 => 76
        85 => 78
        95 => 153
        _ => break
      }
    }
    154 => {
      continue match lexbuf.next_as_int() {
        46 => 156
        _ => break
      }
    }
    155 => {
      _tag_143 = lexbuf.curr_pos()
      _tag_147 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_146
        _capture_0_end = _tag_147
      }
      continue match lexbuf.next_as_int() {
        46 => 154
        48..=55 => 155
        76 => 76
        78 => 76
        85 => 78
        95 => 155
        _ => break
      }
    }
    156 => {
      if matched >= 62 {
        matched = 62
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_142
        _capture_0_end = _tag_143
      }
      break
    }
    157 => {
      _tag_145 = lexbuf.curr_pos()
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_144
        _capture_0_end = _tag_145
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 157
        69 => 158
        95 => 157
        101 => 158
        _ => break
      }
    }
    158 => {
      continue match lexbuf.next_as_int() {
        43 => 217
        45 => 217
        48..=57 => 218
        _ => break
      }
    }
    159 => {
      continue match lexbuf.next_as_int() {
        128 => 80
        _ => break
      }
    }
    160 => {
      continue match lexbuf.next_as_int() {
        191 => 80
        _ => break
      }
    }
    161 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 80
        175 => 80
        _ => break
      }
    }
    162 => {
      continue match lexbuf.next_as_int() {
        159 => 80
        _ => break
      }
    }
    163 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 90
        _ => break
      }
    }
    164 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 163
        _ => break
      }
    }
    165 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 164
        _ => break
      }
    }
    166 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 164
        _ => break
      }
    }
    167 => {
      continue match lexbuf.next_as_int() {
        65..=90 => 93
        95 => 93
        97..=122 => 93
        _ => break
      }
    }
    168 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 97
        _ => break
      }
    }
    169 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 97
        _ => break
      }
    }
    170 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 97
        _ => break
      }
    }
    171 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 94
        _ => break
      }
    }
    172 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 113
        _ => break
      }
    }
    173 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 113
        _ => break
      }
    }
    174 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 113
        _ => break
      }
    }
    175 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 111
        _ => break
      }
    }
    176 => {
      _tag_49 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 219
        _ => break
      }
    }
    177 => {
      _tag_49 = lexbuf.curr_pos()
      _tag_52 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        32 => 220
        34 => 220
        39 => 221
        92 => 220
        98 => 220
        110 => 220
        111 => 222
        114 => 220
        116 => 220
        120 => 223
        _ => break
      }
    }
    178 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 130
        174..=191 => 130
        _ => break
      }
    }
    179 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 130
        _ => break
      }
    }
    180 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 130
        _ => break
      }
    }
    181 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 179
        184..=187 => 179
        _ => break
      }
    }
    182 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 179
        133..=134 => 179
        186..=187 => 179
        191 => 227
        _ => break
      }
    }
    183 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 179
        135..=191 => 179
        _ => break
      }
    }
    184 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 179
        _ => break
      }
    }
    185 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 179
        _ => break
      }
    }
    186 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 179
        _ => break
      }
    }
    187 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 179
        184..=185 => 179
        _ => break
      }
    }
    188 => {
      continue match lexbuf.next_as_int() {
        159 => 225
        160..=174 => 184
        175 => 226
        176..=178 => 184
        _ => break
      }
    }
    189 => {
      continue match lexbuf.next_as_int() {
        160 => 224
        _ => break
      }
    }
    190 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 133
        _ => break
      }
    }
    191 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 133
        _ => break
      }
    }
    192 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 133
        _ => break
      }
    }
    193 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 131
        _ => break
      }
    }
    194 => {
      _tag_87 = lexbuf.curr_pos()
      if matched >= 34 {
        matched = 34
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_86
        _capture_0_end = _tag_87
      }
      break
    }
    195 => {
      _tag_91 = lexbuf.curr_pos()
      if matched >= 36 {
        matched = 36
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_90
        _capture_0_end = _tag_91
      }
      break
    }
    196 => {
      _tag_89 = lexbuf.curr_pos()
      if matched >= 35 {
        matched = 35
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_88
        _capture_0_end = _tag_89
      }
      break
    }
    197 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 144
        _ => break
      }
    }
    198 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 197
        _ => break
      }
    }
    199 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 198
        _ => break
      }
    }
    200 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 198
        _ => break
      }
    }
    201 => {
      _tag_29 = lexbuf.curr_pos()
      _tag_31 = lexbuf.curr_pos()
      if matched >= 14 {
        matched = 14
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_28
        _capture_0_end = _tag_29
        _capture_1_start = _tag_30
        _capture_1_end = _tag_31
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 201
        11..=12 => 201
        14..=127 => 201
        194..=223 => 202
        224..=239 => 203
        240 => 204
        241..=244 => 205
        _ => break
      }
    }
    202 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 201
        _ => break
      }
    }
    203 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 202
        _ => break
      }
    }
    204 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 203
        _ => break
      }
    }
    205 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 203
        _ => break
      }
    }
    206 => {
      continue match lexbuf.next_as_int() {
        160 => 80
        161..=172 => 94
        174..=191 => 94
        _ => break
      }
    }
    207 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 97
        154 => 159
        184..=187 => 97
        _ => break
      }
    }
    208 => {
      continue match lexbuf.next_as_int() {
        128 => 161
        129 => 229
        130 => 97
        133..=134 => 97
        186..=187 => 97
        191 => 171
        _ => break
      }
    }
    209 => {
      continue match lexbuf.next_as_int() {
        128 => 228
        129..=131 => 97
        135..=191 => 97
        _ => break
      }
    }
    210 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 97
        184..=185 => 97
        187 => 160
        _ => break
      }
    }
    211 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 232
        65..=70 => 232
        97..=102 => 232
        _ => break
      }
    }
    212 => {
      _tag_18 = lexbuf.curr_pos()
      _tag_19 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=57 => 230
        65..=70 => 230
        97..=102 => 230
        125 => 231
        _ => break
      }
    }
    213 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 6 {
        matched = 6
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_10
        _capture_0_end = _tag_11
      }
      break
    }
    214 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 233
        _ => break
      }
    }
    215 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 234
        65..=70 => 234
        97..=102 => 234
        _ => break
      }
    }
    216 => {
      _tag_145 = lexbuf.curr_pos()
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_144
        _capture_0_end = _tag_145
      }
      continue match lexbuf.next_as_int() {
        46 => 156
        48..=57 => 235
        65..=70 => 235
        80 => 158
        95 => 235
        97..=102 => 235
        112 => 158
        _ => break
      }
    }
    217 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 218
        _ => break
      }
    }
    218 => {
      _tag_145 = lexbuf.curr_pos()
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_144
        _capture_0_end = _tag_145
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 218
        95 => 218
        _ => break
      }
    }
    219 => {
      _tag_51 = lexbuf.curr_pos()
      if matched >= 19 {
        matched = 19
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_48
        _capture_0_end = _tag_49
        _capture_1_start = _tag_50
        _capture_1_end = _tag_51
      }
      break
    }
    220 => {
      _tag_53 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 237
        _ => break
      }
    }
    221 => {
      _tag_51 = lexbuf.curr_pos()
      _tag_53 = lexbuf.curr_pos()
      if matched >= 19 {
        matched = 19
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_48
        _capture_0_end = _tag_49
        _capture_1_start = _tag_50
        _capture_1_end = _tag_51
      }
      continue match lexbuf.next_as_int() {
        39 => 237
        _ => break
      }
    }
    222 => {
      _tag_44 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=51 => 236
        _ => break
      }
    }
    223 => {
      _tag_40 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=57 => 238
        65..=70 => 238
        97..=102 => 238
        _ => break
      }
    }
    224 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 179
        _ => break
      }
    }
    225 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 179
        _ => break
      }
    }
    226 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 179
        _ => break
      }
    }
    227 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 130
        _ => break
      }
    }
    228 => {
      continue match lexbuf.next_as_int() {
        128 => 239
        129..=191 => 94
        _ => break
      }
    }
    229 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 94
        159 => 239
        160..=191 => 94
        _ => break
      }
    }
    230 => {
      _tag_19 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=57 => 230
        65..=70 => 230
        97..=102 => 230
        125 => 231
        _ => break
      }
    }
    231 => {
      continue match lexbuf.next_as_int() {
        39 => 240
        _ => break
      }
    }
    232 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 241
        65..=70 => 241
        97..=102 => 241
        _ => break
      }
    }
    233 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 242
        _ => break
      }
    }
    234 => {
      continue match lexbuf.next_as_int() {
        39 => 243
        _ => break
      }
    }
    235 => {
      _tag_145 = lexbuf.curr_pos()
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_144
        _capture_0_end = _tag_145
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 235
        65..=70 => 235
        80 => 158
        95 => 235
        97..=102 => 235
        112 => 158
        _ => break
      }
    }
    236 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 244
        _ => break
      }
    }
    237 => {
      _tag_55 = lexbuf.curr_pos()
      if matched >= 20 {
        matched = 20
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_52
        _capture_0_end = _tag_53
        _capture_1_start = _tag_54
        _capture_1_end = _tag_55
      }
      break
    }
    238 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 245
        65..=70 => 245
        97..=102 => 245
        _ => break
      }
    }
    239 => {
      _tag_151 = lexbuf.curr_pos()
      _tag_153 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 80
        11..=12 => 80
        32 => 80
        48..=57 => 94
        65..=90 => 94
        95 => 94
        97..=122 => 94
        126 => 95
        194 => 206
        195..=201 => 97
        202 => 98
        225 => 207
        226 => 208
        227 => 209
        228..=233 => 102
        234 => 103
        235..=236 => 102
        237 => 104
        239 => 210
        240 => 106
        243 => 107
        _ => break
      }
    }
    240 => {
      _tag_21 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_18
        _capture_0_end = _tag_19
        _capture_1_start = _tag_20
        _capture_1_end = _tag_21
      }
      break
    }
    241 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 246
        65..=70 => 246
        97..=102 => 246
        _ => break
      }
    }
    242 => {
      continue match lexbuf.next_as_int() {
        39 => 247
        _ => break
      }
    }
    243 => {
      _tag_13 = lexbuf.curr_pos()
      if matched >= 7 {
        matched = 7
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_12
        _capture_0_end = _tag_13
      }
      break
    }
    244 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 248
        _ => break
      }
    }
    245 => {
      _tag_41 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 249
        _ => break
      }
    }
    246 => {
      continue match lexbuf.next_as_int() {
        39 => 250
        _ => break
      }
    }
    247 => {
      _tag_15 = lexbuf.curr_pos()
      if matched >= 8 {
        matched = 8
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_14
        _capture_0_end = _tag_15
      }
      break
    }
    248 => {
      _tag_45 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 251
        _ => break
      }
    }
    249 => {
      _tag_43 = lexbuf.curr_pos()
      if matched >= 17 {
        matched = 17
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_40
        _capture_0_end = _tag_41
        _capture_1_start = _tag_42
        _capture_1_end = _tag_43
      }
      break
    }
    250 => {
      _tag_17 = lexbuf.curr_pos()
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_16
        _capture_0_end = _tag_17
      }
      break
    }
    251 => {
      _tag_47 = lexbuf.curr_pos()
      if matched >= 18 {
        matched = 18
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_44
        _capture_0_end = _tag_45
        _capture_1_start = _tag_46
        _capture_1_end = _tag_47
      }
      break
    }
    _ => panic()
  }

  guard matched <= 69 else {
    // No pattern matched
    panic()
  }

  lexbuf.reset(pos=_end_pos)
  match matched {
    0 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(NEWLINE, start=_start_pos_of_repr, end=_end_pos_of_repr)
      env.current_bol = _end_pos_of_repr
      env.current_line += 1
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    1 => {
      ()

      tokens(lexbuf, env~, preserve_comment~)
    
    }
    2 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(FAT_ARROW, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    3 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(THIN_ARROW, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    4 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr,InterpInvalidComment)
      }
      if env.comment {
        let comment = Comment::{
          content : repr,
          kind : InlineTrailing,
          consumed_by_docstring : @ref.new(false)
        }
        preserve_comment(comment, _start_pos_of_repr, _end_pos_of_repr)
        env.add_token_with_loc(COMMENT(comment), start=_start_pos_of_repr, end=_end_pos_of_repr)
      }
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    5 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = repr
      env.add_token_with_loc(CHAR(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    6 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = repr
      env.add_token_with_loc(CHAR(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    7 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = repr
      env.add_token_with_loc(CHAR(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    8 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = repr
      env.add_token_with_loc(CHAR(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    9 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = repr
      env.add_token_with_loc(CHAR(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    10 => {
      ()
      let _start_pos_of_hex = _capture_0_start
      let _end_pos_of_hex = _capture_0_end
      let hex = lexbuf.get_string(_start_pos_of_hex, _end_pos_of_hex)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if char_for_hex_escape(hex) is None {
        // Overflow
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InvalidEscapeSequence(repr))
      }
      let literal = repr
      env.add_token_with_loc(CHAR(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    11 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let startpos = _start_pos_of_repr
      let tok =
        match string(lexbuf, env~, end_with_newline = false, allow_interp = true, startpos~) {
          [ InterpLit(repr~, ..) ] => STRING(repr)
          interps => INTERP(interps)
        }
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_position(startpos), env.make_position(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    12 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let startpos = _start_pos_of_repr
      let tok =
        match string(lexbuf, env~, end_with_newline = false, allow_interp = false, startpos~) {
          [ InterpLit(repr~, ..) ] => STRING(repr)
          _interps => panic()
        }
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_position(startpos), env.make_position(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    13 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InterpInvalidMultilineString)
      }
      let startpos = _start_pos_of_repr
      let tok = MULTILINE_INTERP(string(lexbuf, env~, end_with_newline = true, allow_interp = true, startpos~))
      let endpos = lexbuf.curr_pos()
      env.add_token(tok, env.make_position(startpos), env.make_position(endpos))
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    14 => {
      ()
      let _start_pos_of_s = _capture_0_start
      let _end_pos_of_s = _capture_0_end
      let s = lexbuf.get_string(_start_pos_of_s, _end_pos_of_s)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InterpInvalidMultilineString)
      }
      env.add_token_with_loc(MULTILINE_STRING(s), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    15 => {
      ()
      let _start_pos_of_attr = _capture_0_start
      let _end_pos_of_attr = _capture_0_end
      let attr = lexbuf.get_string(_start_pos_of_attr, _end_pos_of_attr)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos_of_repr, end = _end_pos_of_repr, InterpInvalidAttribute)
      }
      env.add_token_with_loc(ATTRIBUTE(attr), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    16 => {
      ()
      let _start_pos_of_pkgname = _capture_0_start
      let _end_pos_of_pkgname = _capture_0_end
      let pkgname = lexbuf.get_string(_start_pos_of_pkgname, _end_pos_of_pkgname)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(PACKAGE_NAME(pkgname), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    17 => {
      ()
      let _start_pos_of_hex = _capture_0_start
      let _end_pos_of_hex = _capture_0_end
      let hex = lexbuf.get_string(_start_pos_of_hex, _end_pos_of_hex)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = "\\x" + hex
      env.add_token_with_loc(BYTE(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    18 => {
      ()
      let _start_pos_of_oct = _capture_0_start
      let _end_pos_of_oct = _capture_0_end
      let oct = lexbuf.get_string(_start_pos_of_oct, _end_pos_of_oct)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = "\\o" + oct
      env.add_token_with_loc(BYTE(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    19 => {
      ()
      let _start_pos_of_ascii = _capture_0_start
      let _end_pos_of_ascii = _capture_0_end
      let ascii = lexbuf.get_char(_start_pos_of_ascii, _end_pos_of_ascii)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = ascii
      env.add_token_with_loc(BYTE([literal]), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    20 => {
      ()
      let _start_pos_of_e = _capture_0_start
      let _end_pos_of_e = _capture_0_end
      let e = lexbuf.get_char(_start_pos_of_e, _end_pos_of_e)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let literal = "\\" + [e]
      env.add_token_with_loc(BYTE(literal), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    21 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      invalid_byte(lexbuf, env~, start=_start_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    22 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(AMPERAMPER, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    23 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(AMPER, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    24 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(CARET, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    25 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(LPAREN, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    26 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(RPAREN, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    27 => {
      ()
      let _start_pos_of_op = _capture_0_start
      let _end_pos_of_op = _capture_0_end
      let op = lexbuf.get_char(_start_pos_of_op, _end_pos_of_op)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(AUGMENTED_ASSIGNMENT([op]), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    28 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX3("*"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    29 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX3("/"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    30 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX3("%"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    31 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(COMMA, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    32 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(DOT_LPAREN, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    33 => {
      ()
      let _start_pos_of_digits = _capture_0_start
      let _end_pos_of_digits = _capture_0_end
      let digits = lexbuf.get_string(_start_pos_of_digits, _end_pos_of_digits)
      let _start_pos_of_repr = _capture_1_start
      let _end_pos_of_repr = _capture_1_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      let idx =
        try {
          @strconv.parse_int!(digits)
        } catch {
          StrConvError(_) => {
            env.add_lexing_error(InvalidDotInt(repr), start=_start_pos_of_repr, end=_end_pos_of_repr)
            0
          }
        }
      env.add_token_with_loc(DOT_INT(idx), start=_start_pos_of_repr, end=_end_pos_of_repr, start_offset = 1)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    34 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(ELLIPSIS, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    35 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(RANGE_INCLUSIVE, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    36 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(RANGE_EXCLUSIVE, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    37 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(DOTDOT, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    38 => {
      ()
      let _start_pos_of_name = _capture_0_start
      let _end_pos_of_name = _capture_0_end
      let name = lexbuf.get_string(_start_pos_of_name, _end_pos_of_name)
      let _ = name

      env.add_token_with_loc(DOT_UIDENT(name), start=_start_pos_of_name, end=_end_pos_of_name)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    39 => {
      ()
      let _start_pos_of_name = _capture_0_start
      let _end_pos_of_name = _capture_0_end
      let name = lexbuf.get_string(_start_pos_of_name, _end_pos_of_name)
      let _ = name

      env.add_token_with_loc(DOT_LIDENT(name), start=_start_pos_of_name, end=_end_pos_of_name)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    40 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(COLONCOLON, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    41 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(COLON, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    42 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(real_semicolon, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    43 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX1("=="), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    44 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(EQUAL, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    45 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX1(repr), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    46 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX2(">>"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    47 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX1(">"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    48 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX2("<<"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    49 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX1("<"), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    50 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(LBRACKET, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    51 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(RBRACKET, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    52 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(LBRACE, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    53 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(RBRACE, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    54 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(PIPE, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    55 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(BARBAR, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    56 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(BAR, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    57 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(PLUS, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    58 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(MINUS, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    59 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(QUESTION, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    60 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(INFIX1("!="), start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    61 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_char(_start_pos_of_repr, _end_pos_of_repr)
      let _ = repr

      env.add_token_with_loc(EXCLAMATION, start=_start_pos_of_repr, end=_end_pos_of_repr)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    62 => {
      ()
      let _start_pos_of_integer = _capture_0_start
      let _end_pos_of_integer = _capture_0_end
      let integer = lexbuf.get_string(_start_pos_of_integer, _end_pos_of_integer)
      let _ = integer

      lexbuf.reset(pos=_end_pos_of_integer)
      env.add_token_with_loc(INT(integer), start=_start_pos_of_integer, end=_end_pos_of_integer)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    63 => {
      ()
      let _start_pos_of_float = _capture_0_start
      let _end_pos_of_float = _capture_0_end
      let float = lexbuf.get_string(_start_pos_of_float, _end_pos_of_float)
      let _ = float

      env.add_token_with_loc(FLOAT(float), start=_start_pos_of_float, end=_end_pos_of_float)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    64 => {
      ()
      let _start_pos_of_integer = _capture_0_start
      let _end_pos_of_integer = _capture_0_end
      let integer = lexbuf.get_string(_start_pos_of_integer, _end_pos_of_integer)
      let _ = integer

      env.add_token_with_loc(INT(integer), start=_start_pos_of_integer, end=_end_pos_of_integer)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    65 => {
      ()

      let end = lexbuf.curr_pos()
      env.add_token_with_loc(EOF, start=end, end=end)
      match env.docstrings {
        None => ()
        Some(c) => {
          if not(c.is_empty()) {
            let last_idx = c.length() - 1
            c[last_idx] = c[last_idx].rev()
          }
          c.rev_inplace()
        }
      }
      return env.arr
    
    }
    66 => {
      ()
      let _start_pos_of_raw = _capture_0_start
      let _end_pos_of_raw = _capture_0_end
      let raw = lexbuf.get_string(_start_pos_of_raw, _end_pos_of_raw)

      env.add_token_with_loc(UIDENT(raw), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    67 => {
      ()
      let _start_pos_of_raw = _capture_0_start
      let _end_pos_of_raw = _capture_0_end
      let raw = lexbuf.get_string(_start_pos_of_raw, _end_pos_of_raw)

      env.add_token_with_loc(POST_LABEL(raw), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    68 => {
      ()
      let _start_pos_of_raw = _capture_0_start
      let _end_pos_of_raw = _capture_0_end
      let raw = lexbuf.get_string(_start_pos_of_raw, _end_pos_of_raw)

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
    69 => {
      ()
      let _start_pos_of_c = _capture_0_start
      let _end_pos_of_c = _capture_0_end
      let c = lexbuf.get_char(_start_pos_of_c, _end_pos_of_c)

      env.add_lexing_error(IllegalCharacter(c), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    _ => panic()
  }
}





