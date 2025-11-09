Unclosed parenthesis
  $ cat > example.rs <<EOF
  > fn main () {
  >   let x = (y));
  >   3 + 3;
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: unexpected `)`
   --> example.rs:2:14
    |
  2 |   let x = (y));
    |              ^ unexpected `)`
