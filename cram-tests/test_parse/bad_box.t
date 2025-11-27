Missing opening bracket for if expression
  $ cat > example.rs <<EOF
  > fn foo() {
  >     Box::new(2 + 3 {3);
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `)`
   --> example.rs:2:20
    |
  2 |     Box::new(2 + 3 {3);
    |                    ^ expected `)`
