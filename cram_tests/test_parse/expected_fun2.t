Non-function appearing at top-level before any functions
  $ cat > example.rs <<EOF
  > 
  >  let x = 3;
  > fn main () {
  >     let x = true;
  >     if x { x }
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected function
   --> example.rs:2:2
    |
  2 |  let x = 3;
    |  ^^^ expected function
