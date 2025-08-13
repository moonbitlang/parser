///|
test "Location::merge - basic merge with different lines" {
  // Location 1: line 5-6, Location 2: line 8-9
  // Expected: merged location from line 5 to line 9
  let loc1 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 5, bol: 50, cnum: 55 },
    end: Position::{ fname: "test.mbt", lnum: 6, bol: 60, cnum: 65 },
  }
  let loc2 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 8, bol: 80, cnum: 85 },
    end: Position::{ fname: "test.mbt", lnum: 9, bol: 90, cnum: 95 },
  }
  inspect(loc1, content="test.mbt:5-5-6:5")
  inspect(loc2, content="test.mbt:8-5-9:5")
  inspect(loc1.merge(loc2), content="test.mbt:5-5-9:5")
  inspect(loc2.merge(loc1), content="test.mbt:5-5-9:5")
}

///|
test "Location::merge - overlapping locations" {
  // Location 1: line 5-8, Location 2: line 6-10
  // Expected: merged location from line 5 to line 10
  let loc1 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 5, bol: 50, cnum: 55 },
    end: Position::{ fname: "test.mbt", lnum: 8, bol: 80, cnum: 85 },
  }
  let loc2 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 6, bol: 60, cnum: 65 },
    end: Position::{ fname: "test.mbt", lnum: 10, bol: 100, cnum: 105 },
  }
  inspect(loc1, content="test.mbt:5-5-8:5")
  inspect(loc2, content="test.mbt:6-5-10:5")
  inspect(loc1.merge(loc2), content="test.mbt:5-5-10:5")
  inspect(loc2.merge(loc1), content="test.mbt:5-5-10:5")
}

///|
test "Location::merge - same line different bol for start positions" {
  // Both locations start on same line but different bol
  // Expected: take the one with smaller bol for start
  let loc1 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 5, bol: 60, cnum: 65 },
    end: Position::{ fname: "test.mbt", lnum: 6, bol: 70, cnum: 75 },
  }
  let loc2 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 5, bol: 50, cnum: 55 },
    end: Position::{ fname: "test.mbt", lnum: 7, bol: 80, cnum: 85 },
  }
  inspect(loc1, content="test.mbt:5-5-6:5")
  inspect(loc2, content="test.mbt:5-5-7:5")
  inspect(loc1.merge(loc2), content="test.mbt:5-5-7:5")
  inspect(loc2.merge(loc1), content="test.mbt:5-5-7:5")
}

///|
test "Location::merge - same line different bol for end positions" {
  // Both locations end on same line but different bol
  // Expected: take the one with larger bol for end
  let loc1 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 3, bol: 30, cnum: 35 },
    end: Position::{ fname: "test.mbt", lnum: 5, bol: 60, cnum: 65 },
  }
  let loc2 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 4, bol: 40, cnum: 45 },
    end: Position::{ fname: "test.mbt", lnum: 5, bol: 70, cnum: 75 },
  }
  inspect(loc1, content="test.mbt:3-5-5:5")
  inspect(loc2, content="test.mbt:4-5-5:5")
  inspect(loc1.merge(loc2), content="test.mbt:3-5-5:5")
  inspect(loc2.merge(loc1), content="test.mbt:3-5-5:5")
}

///|
test "Location::merge - identical locations" {
  // Merging identical locations should return the same location
  let loc = Location::{
    start: Position::{ fname: "test.mbt", lnum: 5, bol: 50, cnum: 55 },
    end: Position::{ fname: "test.mbt", lnum: 6, bol: 60, cnum: 65 },
  }
  inspect(loc, content="test.mbt:5-5-6:5")
  inspect(loc.merge(loc), content="test.mbt:5-5-6:5")
}

///|
test "Location::merge - single character locations" {
  // Test merging single character locations
  let loc1 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 5 },
    end: Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 5 },
  }
  let loc2 = Location::{
    start: Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 10 },
    end: Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 10 },
  }
  let merged = loc1.merge(loc2)

  // Should span from first character to second character
  inspect(loc1, content="test.mbt:1-5-1:5")
  inspect(loc2, content="test.mbt:1-10-1:10")
  inspect(merged, content="test.mbt:1-10-1:10")
}
