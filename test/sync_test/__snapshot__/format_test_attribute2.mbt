
#alert(  category1,   "reason")
#intrinsic("%some.intrinsic"  )
#coverage.skip
#gen_js(id1,id2(id3,id4),   key1=id5)
fn recognized_attributes() -> Unit {}

#unknown1(id1,    "reason")
#unknown2(id1,    id2(id3,id4),   key1=id5,key2=id6)
fn unknown_attributes() -> Unit {}

#custom1.name("abc")
#custom2.name(@qual.name, key=@qual.name, "abc")
fn f() -> Unit {}

#custom.name(@qual.Type)
fn package_qualified_type() -> Unit {}

#custom.name([a,b,c])
fn array_argument() -> Unit {}





