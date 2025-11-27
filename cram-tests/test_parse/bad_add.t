Invalid closing paren
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar (x: (bool, bool), y: &mut i32) {
  >     3 + 2 / 3 (true + 3;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: unexpected token within expression
   --> example.rs:5:15
    |
  5 |     3 + 2 / 3 (true + 3;
    |               ^ unexpected token within expression
