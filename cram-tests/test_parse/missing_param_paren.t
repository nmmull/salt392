Missing opening parenthesis for function
  $ cat > example.rs <<EOF
  > fn main ) { }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `(`
   --> example.rs:1:9
    |
  1 | fn main ) { }
    |         ^ expected `(`
