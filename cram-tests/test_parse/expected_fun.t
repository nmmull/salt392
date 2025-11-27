Non-function appearing at top level after function
  $ cat > example.rs <<EOF
  > fn main () {
  >     let x = true;
  >     if x { x }
  > }
  > true
  > EOF
  $ ./parse_test.exe example.rs
  error: expected function
   --> example.rs:5:1
    |
  5 | true
    | ^^^^ expected function
