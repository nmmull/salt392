Type not given for mutable borrow
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar (x: bool, y: &mut 33) -> bool {
  >     false
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected type
   --> example.rs:4:26
    |
  4 | fn bar (x: bool, y: &mut 33) -> bool {
    |                          ^^ expected type
