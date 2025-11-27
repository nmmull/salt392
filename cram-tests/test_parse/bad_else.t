Trailing expression
  $ cat > example.rs <<EOF
  > fn foo (x: i32) {
  >     if x == 0 {
  >         assert!(x == 0);
  >     }
  >     + 3234
  > }
  > fn main () {
  >   let x = foo(0, .
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected expression or statement or `else`
   --> example.rs:5:5
    |
  5 |     + 3234
    |     ^ expected expression or statement or `else`
