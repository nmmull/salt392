Type not given for borrow
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar (x: bool, y: &33) -> bool {
  >     false
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected type
   --> example.rs:4:22
    |
  4 | fn bar (x: bool, y: &33) -> bool {
    |                      ^^ expected type
