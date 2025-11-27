Unclosed parenthesis
  $ cat > example.rs <<EOF
  > fn foo (x: i32) {
  >     let y = (2;
  > }
  > fn main () {
  >   let x = foo(0);
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `,` or `)`
   --> example.rs:2:15
    |
  2 |     let y = (2;
    |               ^ expected `,` or `)`
