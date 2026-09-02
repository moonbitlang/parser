///|
fn main {
  let a = NestedLeaf::{ number: 1, text: "a" }
  let b = NestedLeaf::{ number: 2, text: "b" }
  let pair = NestedPair::{ left: a, right: b }
  let box = NestedBox::{ pair, choice: Text("c"), tail: 64 }
  check(box.pair.left.number == 1)
  check(box.pair.left.text == "a")
  check(box.tail == 64)
  check(nested_choice_code(box) == 3)
  check(nested_choice_code({ ..box, choice: Number(7) }) == 7)
  let pair_box = { ..box, choice: Pair(pair) }
  check(nested_choice_code(pair_box) == 2)

  let boxes : FixedArray[NestedBox] = [box, pair_box]
  check(boxes[1].pair.right.number == 2)
  let repeated = FixedArray::make(3, box)
  check(repeated[2].pair.left.text == "a")
  check(FixedArray::make(0, box).length() == 0)

  let scalar_box = ScalarBox::{ leaf: { x: 8, y: 9 }, flag: true }
  let scalar_copies = FixedArray::make(2, scalar_box)
  check(scalar_copies[1].leaf.x == 8)
  check(scalar_copies[1].leaf.y == 9)
  check(scalar_copies[1].flag)

  let choice_box = ChoiceBox::{ choice: Text("only") }
  let choice_envelope = ChoiceEnvelope::{ box: choice_box }
  let choice_copies = FixedArray::make(2, choice_envelope)
  check(match choice_copies[1].box.choice {
    Text(value) => value == "only"
    _ => false
  })
  check(FixedArray::make(0, choice_envelope).length() == 0)

  let payloads : FixedArray[StructPayload] = [Empty, Leaf(a), Pair(pair)]
  check(match payloads[0] {
    Empty => true
    _ => false
  })
  check(match payloads[1] {
    Leaf(value) => value.number == 1
    _ => false
  })
  check(match payloads[2] {
    Pair(value) => value.right.number == 2
    _ => false
  })

  // Trait objects occupy two flattened slots (method table and self). Keep
  // different concrete implementations together through nested value structs
  // so a misplaced slot is observable through dynamic dispatch.
  let plain_object = PlainObject::{ code: 10, payload: "plain" } as &NestedValue
  let offset_object = OffsetObject::{ code: 42, payload: "offset" } as &NestedValue
  let trait_pair = TraitPair::{
    left: TraitLeaf::{ prefix: 11L, object: plain_object, suffix: 1 },
    right: TraitLeaf::{ prefix: 22L, object: offset_object, suffix: 2 },
  }
  let trait_envelope = TraitEnvelope::{
    tag: 3,
    pair: trait_pair,
    choice: Direct(33L, plain_object),
  }
  check(trait_envelope.tag == 3)
  check(trait_envelope.pair.left.prefix == 11L)
  check(trait_envelope.pair.right.prefix == 22L)
  check(trait_leaf_code(trait_envelope.pair.left) == 1501)
  check(trait_leaf_code(trait_envelope.pair.right) == 104802)
  check(match trait_envelope.choice {
    Direct(prefix, object) => prefix == 33L && object.value() == 15
    _ => false
  })

  // Exercise bulk retain/release for a value whose nested fields contain both
  // direct trait objects and a value enum containing another trait object.
  let trait_copies = FixedArray::make(3, trait_envelope)
  check(trait_leaf_code(trait_copies[2].pair.right) == 104802)
  check(trait_choice_code(trait_copies[2].choice) == 15)
  check(FixedArray::make(0, trait_envelope).length() == 0)

  // Exercise tag-dependent RC when overwriting a value enum variant containing
  // a direct trait object with one containing a nested value struct.
  let trait_choices : FixedArray[TraitChoice] = FixedArray::make(
    3,
    Direct(44L, offset_object),
  )
  trait_choices[1] = Wrapped(trait_pair.left)
  trait_choices[2] = NoObject
  check(match trait_choices[0] {
    Direct(prefix, object) => prefix == 44L && object.value() == 1048
    _ => false
  })
  check(trait_choice_code(trait_choices[1]) == 1501)
  check(trait_choice_code(trait_choices[2]) == -1)

  // The returned value must retain each nested trait-object self after the
  // FixedArray::make source and its initializer have gone out of scope.
  let retained = retained_trait_envelope(7)
  check(trait_leaf_code(retained.pair.left) == 1107)
  check(trait_leaf_code(retained.pair.right) == 101108)
  check(trait_choice_code(retained.choice) == 1107)

  // Copying the flattened value out of a heap object must keep its nested
  // trait-object selves alive after that object is dropped.
  let retained_from_heap = retained_trait_envelope_from_heap(9)
  check(trait_leaf_code(retained_from_heap.pair.left) == 1309)
  check(trait_leaf_code(retained_from_heap.pair.right) == 101310)
  check(trait_choice_code(retained_from_heap.choice) == 1309)
}

///|
#valtype
struct NestedLeaf {
  number : Int
  text : String
}

///|
#valtype
struct NestedPair {
  left : NestedLeaf
  right : NestedLeaf
}

///|
#valtype
enum NestedChoice {
  Number(Int)
  Text(String)
  Pair(NestedPair)
}

///|
#valtype
struct NestedBox {
  pair : NestedPair
  choice : NestedChoice
  tail : Int64
}

///|
#valtype
struct ScalarLeaf {
  x : Int
  y : Int64
}

///|
#valtype
struct ScalarBox {
  leaf : ScalarLeaf
  flag : Bool
}

///|
#valtype
struct ChoiceBox {
  choice : NestedChoice
}

///|
#valtype
struct ChoiceEnvelope {
  box : ChoiceBox
}

///|
#valtype
enum StructPayload {
  Empty
  Leaf(NestedLeaf)
  Pair(NestedPair)
}

///|
trait NestedValue {
  fn value(Self) -> Int
}

///|
struct PlainObject {
  code : Int
  payload : String
}

///|
impl NestedValue for PlainObject with value(self) {
  self.code + self.payload.length()
}

///|
struct OffsetObject {
  code : Int
  payload : String
}

///|
impl NestedValue for OffsetObject with value(self) {
  1000 + self.code + self.payload.length()
}

///|
#valtype
struct TraitLeaf {
  prefix : Int64
  object : &NestedValue
  suffix : Int
}

///|
#valtype
struct TraitPair {
  left : TraitLeaf
  right : TraitLeaf
}

///|
#valtype
enum TraitChoice {
  NoObject
  Direct(Int64, &NestedValue)
  Wrapped(TraitLeaf)
}

///|
#valtype
struct TraitEnvelope {
  tag : Int
  pair : TraitPair
  choice : TraitChoice
}

///|
struct TraitHeapBox {
  value : TraitEnvelope
}

///|
fn trait_leaf_code(leaf : TraitLeaf) -> Int {
  leaf.object.value() * 100 + leaf.suffix
}

///|
fn trait_choice_code(choice : TraitChoice) -> Int {
  match choice {
    NoObject => -1
    Direct(_, object) => object.value()
    Wrapped(leaf) => trait_leaf_code(leaf)
  }
}

///|
fn retained_trait_envelope(seed : Int) -> TraitEnvelope {
  let plain_object = PlainObject::{ code: seed, payload: "kept" } as &NestedValue
  let offset_object = OffsetObject::{ code: seed, payload: "keep" } as &NestedValue
  let left = TraitLeaf::{ prefix: 1L, object: plain_object, suffix: seed }
  let right = TraitLeaf::{ prefix: 2L, object: offset_object, suffix: seed + 1 }
  let init = TraitEnvelope::{
    tag: seed,
    pair: TraitPair::{ left, right },
    choice: Wrapped(left),
  }
  let copies : FixedArray[TraitEnvelope] = FixedArray::make(3, init)
  copies[1]
}

///|
fn retained_trait_envelope_from_heap(seed : Int) -> TraitEnvelope {
  let box = TraitHeapBox::{ value: retained_trait_envelope(seed) }
  box.value
}

///|
fn nested_choice_code(box : NestedBox) -> Int {
  match box.choice {
    Number(value) => value
    Text(value) => if value == "c" { 3 } else { -1 }
    Pair(value) => value.right.number
  }
}

///|
fn check(condition : Bool) -> Unit {
  if !condition {
    abort("nested value type layout failure")
  }
}
