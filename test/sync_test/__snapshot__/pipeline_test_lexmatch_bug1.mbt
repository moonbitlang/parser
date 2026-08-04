///|
fn init {
  let _ = lexscan "" with longest {
    (
      (re"^[0-9]{2}" as _hour_str) + re":" +
      (re"[0-9]{2}$" as _minute_str)
    ) => 1
    (
      (re"^[0-9]{2}" as _hour_str) + re":" +
      (re"[0-9]{2}" as _minute_str) + re":" +
      (re"[0-9]{2}$" as _second_str)
    ) => 2
    (
      (re"^[0-9]{2}" as _hour_str) + re":" +
      (re"[0-9]{2}" as _minute_str) + re":" +
      (re"[0-9]{2}" as _second_str) + re"\." +
      (re"[0-9]{1,9}$" as _nanosecond_str)
    ) => 3
    _ => 4
  }

}
