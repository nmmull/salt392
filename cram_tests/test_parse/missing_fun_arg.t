Trailing expression
  $ cat > example.rs <<EOF
  > fn foo (x: i32) {
  > }
  > fn main () {
  >   let x = foo(0, .
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected expression or `)`
   --> example.rs:4:18
    |
  4 |   let x = foo(0, .
    |                  ^ expected expression or `)`
