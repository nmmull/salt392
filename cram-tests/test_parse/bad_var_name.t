Non-identifier used for variable name
  $ cat > example.rs <<EOF
  > fn main () {
  >     let x = true;
  >     if x { x }
  >     let mut mut = 255;
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected identifier
   --> example.rs:4:13
    |
  4 |     let mut mut = 255;
    |             ^^^ expected identifier
