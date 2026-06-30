fn main {
  let _ = writer + value <+ "template"
  let _ = maybe + value <? "template"
  let _ = writer <+ "template" + value
  let _ = maybe <? "template" + value
}
