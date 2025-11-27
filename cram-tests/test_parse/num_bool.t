Number followed by boolean
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar (x: (bool, bool), y: &mut i32) {
  >     3 true;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: unexpected token within expression
   --> example.rs:5:7
    |
  5 |     3 true;
    |       ^^^^ unexpected token within expression
