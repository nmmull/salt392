Unclosed parenthesis
  $ cat > example.rs <<EOF
  > fn main () {
  >   (2;
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `,` or `)`
   --> example.rs:2:5
    |
  2 |   (2;
    |     ^ expected `,` or `)`
