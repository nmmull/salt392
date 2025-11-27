Unclosed parenthesis
  $ cat > example.rs <<EOF
  > fn main () {
  >   foo(y;
  >   3 + 3;
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `,` or `)`
   --> example.rs:2:8
    |
  2 |   foo(y;
    |        ^ expected `,` or `)`
