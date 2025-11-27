Accessor index is not a number
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar (x: (bool, bool), y: &mut i32) {
  >     x.true;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected accessor index
   --> example.rs:5:7
    |
  5 |     x.true;
    |       ^^^^ expected accessor index
