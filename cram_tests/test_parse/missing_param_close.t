Missing closing parenthesis
  $ cat > example.rs <<EOF
  > fn foo(x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar(x: bool, y: &mut i32) -> bool {
  >     false
  > }
  > fn baz(okay: bool { true }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `,` or `)`
   --> example.rs:7:19
    |
  7 | fn baz(okay: bool { true }
    |                   ^ expected `,` or `)`
