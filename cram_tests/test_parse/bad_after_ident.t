Boolean after identifier
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar (x: (bool, bool), y: &mut i32) {
  >     x;
  >     x false;
  >     y;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: unexpected token after identifier
   --> example.rs:6:7
    |
  6 |     x false;
    |       ^^^^^ unexpected token after identifier
