Missing opening bracket for if expression
  $ cat > example.rs <<EOF
  > fn foo() {
  >     assert!(2 + 3 {3);
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `)`
   --> example.rs:2:19
    |
  2 |     assert!(2 + 3 {3);
    |                   ^ expected `)`
