Unclosed parenthesis
  $ cat > example.rs <<EOF
  > fn main () {
  >   foo(x,;
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected expression or `)`
   --> example.rs:2:9
    |
  2 |   foo(x,;
    |         ^ expected expression or `)`
