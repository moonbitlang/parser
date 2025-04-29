
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
  let mut _tag_0 = -1
  let mut _tag_1 = -1
  let mut _tag_2 = -1
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
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
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    2 => {
      _tag_2 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_2
      }
      break
    }
    3 => {
      _tag_2 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_2
      }
      continue match lexbuf.next_as_int() {
        9 => 21
        11..=12 => 21
        32 => 21
        125 => 22
        194 => 23
        225 => 24
        226 => 25
        227 => 26
        239 => 27
        _ => break
      }
    }
    4 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    5 => {
      _tag_2 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    6 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    7 => {
      _tag_2 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
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
        154 => 19
        155..=191 => 9
        _ => break
      }
    }
    12 => {
      continue match lexbuf.next_as_int() {
        128 => 28
        129 => 29
        130..=191 => 9
        _ => break
      }
    }
    13 => {
      continue match lexbuf.next_as_int() {
        128 => 19
        129..=191 => 9
        _ => break
      }
    }
    14 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 9
        160..=191 => 20
        _ => break
      }
    }
    15 => {
      continue match lexbuf.next_as_int() {
        128..=186 => 9
        187 => 18
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
        128..=191 => 6
        _ => break
      }
    }
    21 => {
      continue match lexbuf.next_as_int() {
        9 => 21
        11..=12 => 21
        32 => 21
        125 => 22
        194 => 23
        225 => 24
        226 => 25
        227 => 26
        239 => 27
        _ => break
      }
    }
    22 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    23 => {
      continue match lexbuf.next_as_int() {
        160 => 21
        _ => break
      }
    }
    24 => {
      continue match lexbuf.next_as_int() {
        154 => 30
        _ => break
      }
    }
    25 => {
      continue match lexbuf.next_as_int() {
        128 => 32
        129 => 33
        _ => break
      }
    }
    26 => {
      continue match lexbuf.next_as_int() {
        128 => 30
        _ => break
      }
    }
    27 => {
      continue match lexbuf.next_as_int() {
        187 => 31
        _ => break
      }
    }
    28 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 3
        139..=174 => 2
        175 => 3
        176..=191 => 2
        _ => break
      }
    }
    29 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 2
        159 => 3
        160..=191 => 2
        _ => break
      }
    }
    30 => {
      continue match lexbuf.next_as_int() {
        128 => 21
        _ => break
      }
    }
    31 => {
      continue match lexbuf.next_as_int() {
        191 => 21
        _ => break
      }
    }
    32 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 21
        175 => 21
        _ => break
      }
    }
    33 => {
      continue match lexbuf.next_as_int() {
        159 => 21
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
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
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
      _tag_1 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    3 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        10 => 10
        _ => break
      }
    }
    4 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    5 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
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
      _tag_2 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_2
      }
      break
    }
    12 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_2 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_3
      }
      break
    }
    13 => {
      _tag_2 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_2
      }
      continue match lexbuf.next_as_int() {
        0..=47 => 33
        48..=51 => 34
        52..=127 => 33
        194..=223 => 35
        224..=239 => 36
        240 => 37
        241..=244 => 38
        _ => break
      }
    }
    14 => {
      _tag_2 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_2
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 39
        65..=70 => 39
        97..=102 => 39
        123 => 40
        _ => break
      }
    }
    15 => {
      _tag_2 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_2
      }
      continue match lexbuf.next_as_int() {
        0..=47 => 27
        48..=57 => 28
        58..=64 => 27
        65..=70 => 28
        71..=96 => 27
        97..=102 => 28
        103..=127 => 27
        194..=223 => 29
        224..=239 => 30
        240 => 31
        241..=244 => 32
        _ => break
      }
    }
    16 => {
      _tag_4 = lexbuf.curr_pos()
      _tag_2 = lexbuf.curr_pos()
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_4
      }
      continue match lexbuf.next_as_int() {
        9 => 21
        11..=12 => 21
        32 => 21
        194 => 22
        225 => 23
        226 => 24
        227 => 25
        239 => 26
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
      _tag_4 = lexbuf.curr_pos()
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_4
      }
      continue match lexbuf.next_as_int() {
        9 => 21
        11..=12 => 21
        32 => 21
        194 => 22
        225 => 23
        226 => 24
        227 => 25
        239 => 26
        _ => break
      }
    }
    22 => {
      continue match lexbuf.next_as_int() {
        160 => 21
        _ => break
      }
    }
    23 => {
      continue match lexbuf.next_as_int() {
        154 => 41
        _ => break
      }
    }
    24 => {
      continue match lexbuf.next_as_int() {
        128 => 42
        129 => 43
        _ => break
      }
    }
    25 => {
      continue match lexbuf.next_as_int() {
        128 => 41
        _ => break
      }
    }
    26 => {
      continue match lexbuf.next_as_int() {
        187 => 44
        _ => break
      }
    }
    27 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 45
        194..=223 => 47
        224..=239 => 48
        240 => 49
        241..=244 => 50
        _ => break
      }
    }
    28 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 45
        48..=57 => 46
        58..=64 => 45
        65..=70 => 46
        71..=96 => 45
        97..=102 => 46
        103..=127 => 45
        194..=223 => 47
        224..=239 => 48
        240 => 49
        241..=244 => 50
        _ => break
      }
    }
    29 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 27
        _ => break
      }
    }
    30 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 29
        _ => break
      }
    }
    31 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 30
        _ => break
      }
    }
    32 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 30
        _ => break
      }
    }
    33 => {
      continue match lexbuf.next_as_int() {
        0..=127 => 51
        194..=223 => 52
        224..=239 => 53
        240 => 54
        241..=244 => 55
        _ => break
      }
    }
    34 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 51
        48..=55 => 56
        56..=127 => 51
        194..=223 => 52
        224..=239 => 53
        240 => 54
        241..=244 => 55
        _ => break
      }
    }
    35 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 33
        _ => break
      }
    }
    36 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 35
        _ => break
      }
    }
    37 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 36
        _ => break
      }
    }
    38 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 36
        _ => break
      }
    }
    39 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 57
        65..=70 => 57
        97..=102 => 57
        _ => break
      }
    }
    40 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 58
        11..=12 => 58
        14..=47 => 58
        48..=57 => 59
        58..=64 => 58
        65..=70 => 59
        71..=96 => 58
        97..=102 => 59
        103..=124 => 58
        125 => 60
        126..=127 => 58
        194..=223 => 61
        224..=239 => 62
        240 => 63
        241..=244 => 64
        _ => break
      }
    }
    41 => {
      continue match lexbuf.next_as_int() {
        128 => 21
        _ => break
      }
    }
    42 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 21
        175 => 21
        _ => break
      }
    }
    43 => {
      continue match lexbuf.next_as_int() {
        159 => 21
        _ => break
      }
    }
    44 => {
      continue match lexbuf.next_as_int() {
        191 => 21
        _ => break
      }
    }
    45 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 3 {
        matched = 3
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      break
    }
    46 => {
      _tag_6 = lexbuf.curr_pos()
      _tag_5 = lexbuf.curr_pos()
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_6
      }
      break
    }
    47 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 45
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
        0..=127 => 65
        194..=223 => 66
        224..=239 => 67
        240 => 68
        241..=244 => 69
        _ => break
      }
    }
    52 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 51
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
        144..=191 => 53
        _ => break
      }
    }
    55 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 53
        _ => break
      }
    }
    56 => {
      continue match lexbuf.next_as_int() {
        0..=47 => 65
        48..=55 => 70
        56..=127 => 65
        194..=223 => 66
        224..=239 => 67
        240 => 68
        241..=244 => 69
        _ => break
      }
    }
    57 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 71
        65..=70 => 71
        97..=102 => 71
        _ => break
      }
    }
    58 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 58
        11..=12 => 58
        14..=124 => 58
        125 => 60
        126..=127 => 58
        194..=223 => 61
        224..=239 => 62
        240 => 63
        241..=244 => 64
        _ => break
      }
    }
    59 => {
      continue match lexbuf.next_as_int() {
        0..=9 => 58
        11..=12 => 58
        14..=47 => 58
        48..=57 => 59
        58..=64 => 58
        65..=70 => 59
        71..=96 => 58
        97..=102 => 59
        103..=124 => 58
        125 => 72
        126..=127 => 58
        194..=223 => 61
        224..=239 => 62
        240 => 63
        241..=244 => 64
        _ => break
      }
    }
    60 => {
      _tag_7 = lexbuf.curr_pos()
      if matched >= 8 {
        matched = 8
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_7
      }
      break
    }
    61 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 58
        _ => break
      }
    }
    62 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 61
        _ => break
      }
    }
    63 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 62
        _ => break
      }
    }
    64 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 62
        _ => break
      }
    }
    65 => {
      _tag_8 = lexbuf.curr_pos()
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_8
      }
      break
    }
    66 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 65
        _ => break
      }
    }
    67 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 66
        _ => break
      }
    }
    68 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 67
        _ => break
      }
    }
    69 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 67
        _ => break
      }
    }
    70 => {
      _tag_9 = lexbuf.curr_pos()
      _tag_8 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_9
      }
      break
    }
    71 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 73
        65..=70 => 73
        97..=102 => 73
        _ => break
      }
    }
    72 => {
      _tag_10 = lexbuf.curr_pos()
      _tag_7 = lexbuf.curr_pos()
      if matched >= 7 {
        matched = 7
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_10
      }
      break
    }
    73 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 6 {
        matched = 6
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_11
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

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
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

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
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

      env.add_lexing_error(start=_start_pos, end=_end_pos, InvalidEscapeSequence(repr))
      normal(lexbuf, env~, end_with_newline~, allow_interp~, startpos~)
    
    }
    9 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

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
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

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
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
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
        224..=236 => 5
        237 => 6
        238..=239 => 5
        240 => 7
        241..=244 => 8
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
      _tag_1 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    3 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
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
        128..=159 => 4
        160..=191 => 9
        _ => break
      }
    }
    7 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 5
        _ => break
      }
    }
    8 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 5
        _ => break
      }
    }
    9 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 10
        _ => break
      }
    }
    10 => {
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
      let _start_pos_of_c = _capture_0_start
      let _end_pos_of_c = _capture_0_end
      let c = lexbuf.get_char(_start_pos_of_c, _end_pos_of_c)

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
  loop 0 {
    0 => {
      _tag_0 = lexbuf.curr_pos()
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
      _tag_1 = lexbuf.curr_pos()
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      break
    }
    3 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 61
        11..=12 => 61
        32 => 61
        194 => 62
        225 => 63
        226 => 64
        227 => 65
        239 => 66
        _ => break
      }
    }
    4 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    5 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        10 => 131
        _ => break
      }
    }
    6 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 62 {
        matched = 62
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 146
        _ => break
      }
    }
    7 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 11 {
        matched = 11
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    8 => {
      _tag_2 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        65..=90 => 129
        97..=122 => 129
        124 => 130
        _ => break
      }
    }
    9 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        124 => 98
        _ => break
      }
    }
    10 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 30 {
        matched = 30
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 68
        _ => break
      }
    }
    11 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 23 {
        matched = 23
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        38 => 60
        _ => break
      }
    }
    12 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 122
        11..=12 => 122
        14..=38 => 122
        40..=91 => 122
        92 => 123
        93..=127 => 122
        194..=223 => 124
        224..=239 => 125
        240 => 126
        241..=244 => 127
        _ => break
      }
    }
    13 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 25 {
        matched = 25
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    14 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 26 {
        matched = 26
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    15 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 28 {
        matched = 28
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 68
        _ => break
      }
    }
    16 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 58 {
        matched = 58
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 68
        _ => break
      }
    }
    17 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 31 {
        matched = 31
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    18 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 59 {
        matched = 59
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 68
        62 => 121
        _ => break
      }
    }
    19 => {
      _tag_4 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        40 => 104
        46 => 105
        48..=57 => 106
        65..=90 => 107
        95 => 108
        97..=122 => 108
        194 => 109
        195..=201 => 110
        202 => 111
        225 => 112
        226 => 113
        227 => 114
        228..=233 => 115
        234 => 116
        235..=236 => 115
        237 => 117
        239 => 118
        240 => 119
        243 => 120
        _ => break
      }
    }
    20 => {
      _tag_3 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 29 {
        matched = 29
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        47 => 103
        61 => 68
        _ => break
      }
    }
    21 => {
      _tag_5 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 99
        48..=57 => 100
        66 => 139
        76 => 101
        78 => 101
        79 => 140
        85 => 102
        88 => 141
        95 => 100
        98 => 139
        111 => 140
        120 => 141
        _ => break
      }
    }
    22 => {
      _tag_5 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 99
        48..=57 => 100
        76 => 101
        78 => 101
        85 => 102
        95 => 100
        _ => break
      }
    }
    23 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 41 {
        matched = 41
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        58 => 138
        _ => break
      }
    }
    24 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 42 {
        matched = 42
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    25 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 50 {
        matched = 50
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        60 => 58
        61 => 59
        _ => break
      }
    }
    26 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 44 {
        matched = 44
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 133
        62 => 134
        _ => break
      }
    }
    27 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 48 {
        matched = 48
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        61 => 69
        62 => 70
        _ => break
      }
    }
    28 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 60 {
        matched = 60
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    29 => {
      _tag_6 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 70 {
        matched = 70
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_1
      }
      continue match lexbuf.next_as_int() {
        65..=90 => 67
        95 => 67
        97..=122 => 67
        _ => break
      }
    }
    30 => {
      _tag_7 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 67 {
        matched = 67
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_7
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 71
        65..=90 => 71
        95 => 71
        97..=122 => 71
        194 => 72
        195..=201 => 73
        202 => 74
        225 => 75
        226 => 76
        227 => 77
        228..=233 => 78
        234 => 79
        235..=236 => 78
        237 => 80
        239 => 81
        240 => 82
        243 => 83
        _ => break
      }
    }
    31 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 51 {
        matched = 51
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    32 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 52 {
        matched = 52
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    33 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 24 {
        matched = 24
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    34 => {
      _tag_8 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_8
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 84
        65..=90 => 84
        95 => 84
        97..=122 => 84
        126 => 85
        194 => 86
        195..=201 => 87
        202 => 88
        225 => 89
        226 => 90
        227 => 91
        228..=233 => 92
        234 => 93
        235..=236 => 92
        237 => 94
        239 => 95
        240 => 96
        243 => 97
        _ => break
      }
    }
    35 => {
      _tag_8 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_8
      }
      continue match lexbuf.next_as_int() {
        34 => 144
        39 => 145
        48..=57 => 84
        65..=90 => 84
        95 => 84
        97..=122 => 84
        126 => 85
        194 => 86
        195..=201 => 87
        202 => 88
        225 => 89
        226 => 90
        227 => 91
        228..=233 => 92
        234 => 93
        235..=236 => 92
        237 => 94
        239 => 95
        240 => 96
        243 => 97
        _ => break
      }
    }
    36 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 53 {
        matched = 53
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    37 => {
      _tag_1 = lexbuf.curr_pos()
      if matched >= 57 {
        matched = 57
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        62 => 142
        124 => 143
        _ => break
      }
    }
    38 => {
      _tag_1 = lexbuf.curr_pos()
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
        154 => 128
        155..=183 => 42
        184..=187 => 40
        188..=191 => 42
        _ => break
      }
    }
    45 => {
      continue match lexbuf.next_as_int() {
        128 => 135
        129 => 136
        130 => 40
        131..=132 => 42
        133..=134 => 40
        135..=185 => 42
        186..=187 => 40
        188..=190 => 42
        191 => 137
        _ => break
      }
    }
    46 => {
      continue match lexbuf.next_as_int() {
        128 => 132
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
      if matched >= 49 {
        matched = 49
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    59 => {
      if matched >= 45 {
        matched = 45
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    60 => {
      if matched >= 22 {
        matched = 22
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    61 => {
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 61
        11..=12 => 61
        32 => 61
        194 => 62
        225 => 63
        226 => 64
        227 => 65
        239 => 66
        _ => break
      }
    }
    62 => {
      continue match lexbuf.next_as_int() {
        160 => 61
        _ => break
      }
    }
    63 => {
      continue match lexbuf.next_as_int() {
        154 => 150
        _ => break
      }
    }
    64 => {
      continue match lexbuf.next_as_int() {
        128 => 147
        129 => 148
        _ => break
      }
    }
    65 => {
      continue match lexbuf.next_as_int() {
        128 => 150
        _ => break
      }
    }
    66 => {
      continue match lexbuf.next_as_int() {
        187 => 149
        _ => break
      }
    }
    67 => {
      _tag_9 = lexbuf.curr_pos()
      if matched >= 16 {
        matched = 16
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_6
        _capture_0_end = _tag_9
      }
      continue match lexbuf.next_as_int() {
        47 => 151
        48..=57 => 67
        65..=90 => 67
        95 => 67
        97..=122 => 67
        _ => break
      }
    }
    68 => {
      _tag_10 = lexbuf.curr_pos()
      if matched >= 27 {
        matched = 27
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_3
        _capture_1_start = _tag_0
        _capture_1_end = _tag_10
      }
      break
    }
    69 => {
      if matched >= 46 {
        matched = 46
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    70 => {
      if matched >= 47 {
        matched = 47
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    71 => {
      _tag_7 = lexbuf.curr_pos()
      if matched >= 67 {
        matched = 67
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_7
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 71
        65..=90 => 71
        95 => 71
        97..=122 => 71
        194 => 72
        195..=201 => 73
        202 => 74
        225 => 75
        226 => 76
        227 => 77
        228..=233 => 78
        234 => 79
        235..=236 => 78
        237 => 80
        239 => 81
        240 => 82
        243 => 83
        _ => break
      }
    }
    72 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 71
        174..=191 => 71
        _ => break
      }
    }
    73 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 71
        _ => break
      }
    }
    74 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 71
        _ => break
      }
    }
    75 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 73
        184..=187 => 73
        _ => break
      }
    }
    76 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 73
        133..=134 => 73
        186..=187 => 73
        191 => 155
        _ => break
      }
    }
    77 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 73
        135..=191 => 73
        _ => break
      }
    }
    78 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 73
        _ => break
      }
    }
    79 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 73
        _ => break
      }
    }
    80 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 73
        _ => break
      }
    }
    81 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 73
        184..=185 => 73
        _ => break
      }
    }
    82 => {
      continue match lexbuf.next_as_int() {
        159 => 153
        160..=174 => 78
        175 => 154
        176..=178 => 78
        _ => break
      }
    }
    83 => {
      continue match lexbuf.next_as_int() {
        160 => 152
        _ => break
      }
    }
    84 => {
      _tag_8 = lexbuf.curr_pos()
      if matched >= 69 {
        matched = 69
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_8
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 84
        65..=90 => 84
        95 => 84
        97..=122 => 84
        126 => 85
        194 => 86
        195..=201 => 87
        202 => 88
        225 => 89
        226 => 90
        227 => 91
        228..=233 => 92
        234 => 93
        235..=236 => 92
        237 => 94
        239 => 95
        240 => 96
        243 => 97
        _ => break
      }
    }
    85 => {
      if matched >= 68 {
        matched = 68
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_8
      }
      break
    }
    86 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 84
        174..=191 => 84
        _ => break
      }
    }
    87 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 84
        _ => break
      }
    }
    88 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 84
        _ => break
      }
    }
    89 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 87
        184..=187 => 87
        _ => break
      }
    }
    90 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 87
        133..=134 => 87
        186..=187 => 87
        191 => 159
        _ => break
      }
    }
    91 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 87
        135..=191 => 87
        _ => break
      }
    }
    92 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 87
        _ => break
      }
    }
    93 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 87
        _ => break
      }
    }
    94 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 87
        _ => break
      }
    }
    95 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 87
        184..=185 => 87
        _ => break
      }
    }
    96 => {
      continue match lexbuf.next_as_int() {
        159 => 157
        160..=174 => 92
        175 => 158
        176..=178 => 92
        _ => break
      }
    }
    97 => {
      continue match lexbuf.next_as_int() {
        160 => 156
        _ => break
      }
    }
    98 => {
      if matched >= 13 {
        matched = 13
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    99 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_11
      }
      continue match lexbuf.next_as_int() {
        46 => 160
        48..=57 => 161
        69 => 162
        95 => 161
        101 => 162
        _ => break
      }
    }
    100 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 99
        48..=57 => 100
        76 => 101
        78 => 101
        85 => 102
        95 => 100
        _ => break
      }
    }
    101 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 163
        _ => break
      }
    }
    102 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 163
        76 => 101
        _ => break
      }
    }
    103 => {
      _tag_12 = lexbuf.curr_pos()
      if matched >= 4 {
        matched = 4
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_12
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 103
        11..=12 => 103
        14..=127 => 103
        194..=223 => 164
        224..=239 => 165
        240 => 166
        241..=244 => 167
        _ => break
      }
    }
    104 => {
      if matched >= 32 {
        matched = 32
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    105 => {
      if matched >= 37 {
        matched = 37
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        46 => 168
        60 => 169
        61 => 170
        _ => break
      }
    }
    106 => {
      _tag_13 = lexbuf.curr_pos()
      if matched >= 33 {
        matched = 33
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_13
        _capture_1_start = _tag_0
        _capture_1_end = _tag_13
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 106
        _ => break
      }
    }
    107 => {
      _tag_14 = lexbuf.curr_pos()
      if matched >= 38 {
        matched = 38
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_14
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 107
        65..=90 => 107
        95 => 107
        97..=122 => 107
        194 => 171
        195..=201 => 172
        202 => 173
        225 => 174
        226 => 175
        227 => 176
        228..=233 => 177
        234 => 178
        235..=236 => 177
        237 => 179
        239 => 180
        240 => 181
        243 => 182
        _ => break
      }
    }
    108 => {
      _tag_15 = lexbuf.curr_pos()
      if matched >= 39 {
        matched = 39
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_4
        _capture_0_end = _tag_15
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 108
        65..=90 => 108
        95 => 108
        97..=122 => 108
        194 => 109
        195..=201 => 110
        202 => 111
        225 => 112
        226 => 113
        227 => 114
        228..=233 => 115
        234 => 116
        235..=236 => 115
        237 => 117
        239 => 118
        240 => 119
        243 => 120
        _ => break
      }
    }
    109 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 108
        174..=191 => 108
        _ => break
      }
    }
    110 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 108
        _ => break
      }
    }
    111 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 108
        _ => break
      }
    }
    112 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 110
        184..=187 => 110
        _ => break
      }
    }
    113 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 110
        133..=134 => 110
        186..=187 => 110
        191 => 186
        _ => break
      }
    }
    114 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 110
        135..=191 => 110
        _ => break
      }
    }
    115 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 110
        _ => break
      }
    }
    116 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 110
        _ => break
      }
    }
    117 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 110
        _ => break
      }
    }
    118 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 110
        184..=185 => 110
        _ => break
      }
    }
    119 => {
      continue match lexbuf.next_as_int() {
        159 => 184
        160..=174 => 115
        175 => 185
        176..=178 => 115
        _ => break
      }
    }
    120 => {
      continue match lexbuf.next_as_int() {
        160 => 183
        _ => break
      }
    }
    121 => {
      if matched >= 3 {
        matched = 3
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    122 => {
      continue match lexbuf.next_as_int() {
        39 => 191
        _ => break
      }
    }
    123 => {
      continue match lexbuf.next_as_int() {
        32 => 187
        34 => 187
        39 => 187
        92 => 187
        98 => 187
        110 => 187
        111 => 188
        114 => 187
        116 => 187
        117 => 189
        120 => 190
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
      continue match lexbuf.next_as_int() {
        128..=191 => 124
        _ => break
      }
    }
    126 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 125
        _ => break
      }
    }
    127 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 125
        _ => break
      }
    }
    128 => {
      continue match lexbuf.next_as_int() {
        128 => 3
        129..=191 => 2
        _ => break
      }
    }
    129 => {
      _tag_16 = lexbuf.curr_pos()
      if matched >= 15 {
        matched = 15
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_2
        _capture_0_end = _tag_16
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 129
        11..=12 => 129
        14..=127 => 129
        194..=223 => 192
        224..=239 => 193
        240 => 194
        241..=244 => 195
        _ => break
      }
    }
    130 => {
      _tag_17 = lexbuf.curr_pos()
      _tag_18 = lexbuf.curr_pos()
      if matched >= 14 {
        matched = 14
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_17
        _capture_0_end = _tag_18
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 196
        11..=12 => 196
        14..=127 => 196
        194..=223 => 197
        224..=239 => 198
        240 => 199
        241..=244 => 200
        _ => break
      }
    }
    131 => {
      if matched >= 0 {
        matched = 0
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    132 => {
      continue match lexbuf.next_as_int() {
        128 => 201
        129..=191 => 34
        _ => break
      }
    }
    133 => {
      if matched >= 43 {
        matched = 43
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    134 => {
      if matched >= 2 {
        matched = 2
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    135 => {
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
    136 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 34
        159 => 201
        160..=191 => 34
        _ => break
      }
    }
    137 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 2
        176..=191 => 34
        _ => break
      }
    }
    138 => {
      if matched >= 40 {
        matched = 40
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    139 => {
      continue match lexbuf.next_as_int() {
        48..=49 => 204
        _ => break
      }
    }
    140 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 203
        _ => break
      }
    }
    141 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 202
        65..=70 => 202
        97..=102 => 202
        _ => break
      }
    }
    142 => {
      if matched >= 55 {
        matched = 55
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    143 => {
      if matched >= 56 {
        matched = 56
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    144 => {
      if matched >= 12 {
        matched = 12
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    145 => {
      _tag_19 = lexbuf.curr_pos()
      if matched >= 21 {
        matched = 21
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_19
      }
      continue match lexbuf.next_as_int() {
        0..=91 => 205
        92 => 206
        93..=127 => 205
        _ => break
      }
    }
    146 => {
      if matched >= 61 {
        matched = 61
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    147 => {
      continue match lexbuf.next_as_int() {
        128..=138 => 61
        175 => 61
        _ => break
      }
    }
    148 => {
      continue match lexbuf.next_as_int() {
        159 => 61
        _ => break
      }
    }
    149 => {
      continue match lexbuf.next_as_int() {
        191 => 61
        _ => break
      }
    }
    150 => {
      continue match lexbuf.next_as_int() {
        128 => 61
        _ => break
      }
    }
    151 => {
      continue match lexbuf.next_as_int() {
        65..=90 => 67
        95 => 67
        97..=122 => 67
        _ => break
      }
    }
    152 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 73
        _ => break
      }
    }
    153 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 73
        _ => break
      }
    }
    154 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 73
        _ => break
      }
    }
    155 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 71
        _ => break
      }
    }
    156 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 87
        _ => break
      }
    }
    157 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 87
        _ => break
      }
    }
    158 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 87
        _ => break
      }
    }
    159 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 84
        _ => break
      }
    }
    160 => {
      if matched >= 63 {
        matched = 63
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      break
    }
    161 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_11
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 161
        69 => 162
        95 => 161
        101 => 162
        _ => break
      }
    }
    162 => {
      continue match lexbuf.next_as_int() {
        43 => 207
        45 => 207
        48..=57 => 208
        _ => break
      }
    }
    163 => {
      continue match lexbuf.next_as_int() {
        46 => 160
        _ => break
      }
    }
    164 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 103
        _ => break
      }
    }
    165 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 164
        _ => break
      }
    }
    166 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 165
        _ => break
      }
    }
    167 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 165
        _ => break
      }
    }
    168 => {
      if matched >= 34 {
        matched = 34
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    169 => {
      if matched >= 36 {
        matched = 36
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    170 => {
      if matched >= 35 {
        matched = 35
        _end_pos = lexbuf.curr_pos()
      }
      break
    }
    171 => {
      continue match lexbuf.next_as_int() {
        161..=172 => 107
        174..=191 => 107
        _ => break
      }
    }
    172 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 107
        _ => break
      }
    }
    173 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 107
        _ => break
      }
    }
    174 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 172
        184..=187 => 172
        _ => break
      }
    }
    175 => {
      continue match lexbuf.next_as_int() {
        129..=130 => 172
        133..=134 => 172
        186..=187 => 172
        191 => 212
        _ => break
      }
    }
    176 => {
      continue match lexbuf.next_as_int() {
        128..=131 => 172
        135..=191 => 172
        _ => break
      }
    }
    177 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 172
        _ => break
      }
    }
    178 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 172
        _ => break
      }
    }
    179 => {
      continue match lexbuf.next_as_int() {
        128..=159 => 172
        _ => break
      }
    }
    180 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 172
        184..=185 => 172
        _ => break
      }
    }
    181 => {
      continue match lexbuf.next_as_int() {
        159 => 210
        160..=174 => 177
        175 => 211
        176..=178 => 177
        _ => break
      }
    }
    182 => {
      continue match lexbuf.next_as_int() {
        160 => 209
        _ => break
      }
    }
    183 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 110
        _ => break
      }
    }
    184 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 110
        _ => break
      }
    }
    185 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 110
        _ => break
      }
    }
    186 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 108
        _ => break
      }
    }
    187 => {
      continue match lexbuf.next_as_int() {
        39 => 214
        _ => break
      }
    }
    188 => {
      continue match lexbuf.next_as_int() {
        48..=51 => 215
        _ => break
      }
    }
    189 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 216
        65..=70 => 216
        97..=102 => 216
        123 => 217
        _ => break
      }
    }
    190 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 213
        65..=70 => 213
        97..=102 => 213
        _ => break
      }
    }
    191 => {
      _tag_20 = lexbuf.curr_pos()
      if matched >= 5 {
        matched = 5
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_20
      }
      break
    }
    192 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 129
        _ => break
      }
    }
    193 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 192
        _ => break
      }
    }
    194 => {
      continue match lexbuf.next_as_int() {
        144..=191 => 193
        _ => break
      }
    }
    195 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 193
        _ => break
      }
    }
    196 => {
      _tag_18 = lexbuf.curr_pos()
      if matched >= 14 {
        matched = 14
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_17
        _capture_0_end = _tag_18
      }
      continue match lexbuf.next_as_int() {
        0..=9 => 196
        11..=12 => 196
        14..=127 => 196
        194..=223 => 197
        224..=239 => 198
        240 => 199
        241..=244 => 200
        _ => break
      }
    }
    197 => {
      continue match lexbuf.next_as_int() {
        128..=191 => 196
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
      _tag_8 = lexbuf.curr_pos()
      _tag_1 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 61
        11..=12 => 61
        32 => 61
        48..=57 => 84
        65..=90 => 84
        95 => 84
        97..=122 => 84
        126 => 85
        194 => 218
        195..=201 => 87
        202 => 88
        225 => 219
        226 => 220
        227 => 221
        228..=233 => 92
        234 => 93
        235..=236 => 92
        237 => 94
        239 => 222
        240 => 96
        243 => 97
        _ => break
      }
    }
    202 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 223
        48..=57 => 202
        65..=70 => 202
        76 => 101
        78 => 101
        85 => 102
        95 => 202
        97..=102 => 202
        _ => break
      }
    }
    203 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 163
        48..=55 => 203
        76 => 101
        78 => 101
        85 => 102
        95 => 203
        _ => break
      }
    }
    204 => {
      _tag_5 = lexbuf.curr_pos()
      if matched >= 65 {
        matched = 65
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_5
      }
      continue match lexbuf.next_as_int() {
        46 => 163
        48..=49 => 204
        76 => 101
        78 => 101
        85 => 102
        95 => 204
        _ => break
      }
    }
    205 => {
      _tag_21 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 224
        _ => break
      }
    }
    206 => {
      _tag_21 = lexbuf.curr_pos()
      _tag_22 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        32 => 225
        34 => 225
        39 => 226
        92 => 225
        98 => 225
        110 => 225
        111 => 227
        114 => 225
        116 => 225
        120 => 228
        _ => break
      }
    }
    207 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 208
        _ => break
      }
    }
    208 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_11
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 208
        95 => 208
        _ => break
      }
    }
    209 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 172
        _ => break
      }
    }
    210 => {
      continue match lexbuf.next_as_int() {
        128..=175 => 172
        _ => break
      }
    }
    211 => {
      continue match lexbuf.next_as_int() {
        160..=168 => 172
        _ => break
      }
    }
    212 => {
      continue match lexbuf.next_as_int() {
        176..=191 => 107
        _ => break
      }
    }
    213 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 229
        65..=70 => 229
        97..=102 => 229
        _ => break
      }
    }
    214 => {
      _tag_23 = lexbuf.curr_pos()
      if matched >= 6 {
        matched = 6
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_23
      }
      break
    }
    215 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 230
        _ => break
      }
    }
    216 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 231
        65..=70 => 231
        97..=102 => 231
        _ => break
      }
    }
    217 => {
      _tag_24 = lexbuf.curr_pos()
      _tag_25 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=57 => 232
        65..=70 => 232
        97..=102 => 232
        125 => 233
        _ => break
      }
    }
    218 => {
      continue match lexbuf.next_as_int() {
        160 => 61
        161..=172 => 84
        174..=191 => 84
        _ => break
      }
    }
    219 => {
      continue match lexbuf.next_as_int() {
        132..=135 => 87
        154 => 150
        184..=187 => 87
        _ => break
      }
    }
    220 => {
      continue match lexbuf.next_as_int() {
        128 => 147
        129 => 234
        130 => 87
        133..=134 => 87
        186..=187 => 87
        191 => 159
        _ => break
      }
    }
    221 => {
      continue match lexbuf.next_as_int() {
        128 => 235
        129..=131 => 87
        135..=191 => 87
        _ => break
      }
    }
    222 => {
      continue match lexbuf.next_as_int() {
        164..=171 => 87
        184..=185 => 87
        187 => 149
        _ => break
      }
    }
    223 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_11
      }
      continue match lexbuf.next_as_int() {
        46 => 160
        48..=57 => 236
        65..=70 => 236
        80 => 162
        95 => 236
        97..=102 => 236
        112 => 162
        _ => break
      }
    }
    224 => {
      if matched >= 19 {
        matched = 19
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_19
        _capture_0_end = _tag_21
      }
      break
    }
    225 => {
      _tag_26 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 237
        _ => break
      }
    }
    226 => {
      _tag_26 = lexbuf.curr_pos()
      if matched >= 19 {
        matched = 19
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_19
        _capture_0_end = _tag_21
      }
      continue match lexbuf.next_as_int() {
        39 => 237
        _ => break
      }
    }
    227 => {
      _tag_27 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=51 => 238
        _ => break
      }
    }
    228 => {
      _tag_28 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=57 => 239
        65..=70 => 239
        97..=102 => 239
        _ => break
      }
    }
    229 => {
      continue match lexbuf.next_as_int() {
        39 => 240
        _ => break
      }
    }
    230 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 241
        _ => break
      }
    }
    231 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 242
        65..=70 => 242
        97..=102 => 242
        _ => break
      }
    }
    232 => {
      _tag_25 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        48..=57 => 232
        65..=70 => 232
        97..=102 => 232
        125 => 233
        _ => break
      }
    }
    233 => {
      continue match lexbuf.next_as_int() {
        39 => 243
        _ => break
      }
    }
    234 => {
      continue match lexbuf.next_as_int() {
        128..=158 => 84
        159 => 244
        160..=191 => 84
        _ => break
      }
    }
    235 => {
      continue match lexbuf.next_as_int() {
        128 => 244
        129..=191 => 84
        _ => break
      }
    }
    236 => {
      _tag_11 = lexbuf.curr_pos()
      if matched >= 64 {
        matched = 64
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_11
      }
      continue match lexbuf.next_as_int() {
        48..=57 => 236
        65..=70 => 236
        80 => 162
        95 => 236
        97..=102 => 236
        112 => 162
        _ => break
      }
    }
    237 => {
      if matched >= 20 {
        matched = 20
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_22
        _capture_0_end = _tag_26
      }
      break
    }
    238 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 245
        _ => break
      }
    }
    239 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 246
        65..=70 => 246
        97..=102 => 246
        _ => break
      }
    }
    240 => {
      _tag_29 = lexbuf.curr_pos()
      if matched >= 7 {
        matched = 7
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_29
      }
      break
    }
    241 => {
      continue match lexbuf.next_as_int() {
        39 => 247
        _ => break
      }
    }
    242 => {
      continue match lexbuf.next_as_int() {
        48..=57 => 248
        65..=70 => 248
        97..=102 => 248
        _ => break
      }
    }
    243 => {
      _tag_30 = lexbuf.curr_pos()
      if matched >= 10 {
        matched = 10
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_24
        _capture_0_end = _tag_25
        _capture_1_start = _tag_0
        _capture_1_end = _tag_30
      }
      break
    }
    244 => {
      _tag_8 = lexbuf.curr_pos()
      if matched >= 1 {
        matched = 1
        _end_pos = lexbuf.curr_pos()
      }
      continue match lexbuf.next_as_int() {
        9 => 61
        11..=12 => 61
        32 => 61
        48..=57 => 84
        65..=90 => 84
        95 => 84
        97..=122 => 84
        126 => 85
        194 => 218
        195..=201 => 87
        202 => 88
        225 => 219
        226 => 220
        227 => 221
        228..=233 => 92
        234 => 93
        235..=236 => 92
        237 => 94
        239 => 222
        240 => 96
        243 => 97
        _ => break
      }
    }
    245 => {
      continue match lexbuf.next_as_int() {
        48..=55 => 249
        _ => break
      }
    }
    246 => {
      _tag_31 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 250
        _ => break
      }
    }
    247 => {
      _tag_32 = lexbuf.curr_pos()
      if matched >= 8 {
        matched = 8
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_32
      }
      break
    }
    248 => {
      continue match lexbuf.next_as_int() {
        39 => 251
        _ => break
      }
    }
    249 => {
      _tag_33 = lexbuf.curr_pos()
      continue match lexbuf.next_as_int() {
        39 => 252
        _ => break
      }
    }
    250 => {
      if matched >= 17 {
        matched = 17
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_28
        _capture_0_end = _tag_31
      }
      break
    }
    251 => {
      _tag_34 = lexbuf.curr_pos()
      if matched >= 9 {
        matched = 9
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_0
        _capture_0_end = _tag_34
      }
      break
    }
    252 => {
      if matched >= 18 {
        matched = 18
        _end_pos = lexbuf.curr_pos()
        _capture_0_start = _tag_27
        _capture_0_end = _tag_33
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
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

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
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    6 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    7 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    8 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    9 => {
      ()
      let _start_pos_of_repr = _capture_0_start
      let _end_pos_of_repr = _capture_0_end
      let repr = lexbuf.get_string(_start_pos_of_repr, _end_pos_of_repr)

      env.add_token_with_loc(CHAR(repr), start=_start_pos, end=_end_pos)
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
      let _start_pos_of_s = _capture_0_start
      let _end_pos_of_s = _capture_0_end
      let s = lexbuf.get_string(_start_pos_of_s, _end_pos_of_s)

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos, end=_end_pos, InterpInvalidMultilineString)
      }
      env.add_token_with_loc(MULTILINE_STRING(s), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    15 => {
      ()
      let _start_pos_of_attr = _capture_0_start
      let _end_pos_of_attr = _capture_0_end
      let attr = lexbuf.get_string(_start_pos_of_attr, _end_pos_of_attr)

      if env.is_interpolation {
        env.add_lexing_error(start=_start_pos, end=_end_pos, InterpInvalidAttribute)
      }
      env.add_token_with_loc(ATTRIBUTE(attr), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    16 => {
      ()
      let _start_pos_of_pkgname = _capture_0_start
      let _end_pos_of_pkgname = _capture_0_end
      let pkgname = lexbuf.get_string(_start_pos_of_pkgname, _end_pos_of_pkgname)

      env.add_token_with_loc(PACKAGE_NAME(pkgname), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    17 => {
      ()
      let _start_pos_of_hex = _capture_0_start
      let _end_pos_of_hex = _capture_0_end
      let hex = lexbuf.get_string(_start_pos_of_hex, _end_pos_of_hex)

      let literal = "\\x" + hex
      env.add_token_with_loc(BYTE(literal), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    18 => {
      ()
      let _start_pos_of_oct = _capture_0_start
      let _end_pos_of_oct = _capture_0_end
      let oct = lexbuf.get_string(_start_pos_of_oct, _end_pos_of_oct)

      let literal = "\\o" + oct
      env.add_token_with_loc(BYTE(literal), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    19 => {
      ()
      let _start_pos_of_ascii = _capture_0_start
      let _end_pos_of_ascii = _capture_0_end
      let ascii = lexbuf.get_char(_start_pos_of_ascii, _end_pos_of_ascii)

      let literal = ascii
      env.add_token_with_loc(BYTE([literal]), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    20 => {
      ()
      let _start_pos_of_e = _capture_0_start
      let _end_pos_of_e = _capture_0_end
      let e = lexbuf.get_char(_start_pos_of_e, _end_pos_of_e)

      let literal = "\\" + [e]
      env.add_token_with_loc(BYTE(literal), start=_start_pos, end=_end_pos)
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
      let _start_pos_of_integer = _capture_0_start
      let _end_pos_of_integer = _capture_0_end
      let integer = lexbuf.get_string(_start_pos_of_integer, _end_pos_of_integer)
      let _ = integer

      lexbuf.reset(pos=_end_pos_of_integer)
      env.add_token_with_loc(INT(integer), start=_start_pos_of_integer, end=_end_pos_of_integer)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    64 => {
      ()
      let _start_pos_of_float = _capture_0_start
      let _end_pos_of_float = _capture_0_end
      let float = lexbuf.get_string(_start_pos_of_float, _end_pos_of_float)

      env.add_token_with_loc(FLOAT(float), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    65 => {
      ()
      let _start_pos_of_integer = _capture_0_start
      let _end_pos_of_integer = _capture_0_end
      let integer = lexbuf.get_string(_start_pos_of_integer, _end_pos_of_integer)

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
      let _start_pos_of_raw = _capture_0_start
      let _end_pos_of_raw = _capture_0_end
      let raw = lexbuf.get_string(_start_pos_of_raw, _end_pos_of_raw)

      env.add_token_with_loc(UIDENT(raw), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    68 => {
      ()
      let _start_pos_of_raw = _capture_0_start
      let _end_pos_of_raw = _capture_0_end
      let raw = lexbuf.get_string(_start_pos_of_raw, _end_pos_of_raw)

      env.add_token_with_loc(POST_LABEL(raw), start=_start_pos, end=_end_pos)
      tokens(lexbuf, env~, preserve_comment~)
    
    }
    69 => {
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
    70 => {
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





