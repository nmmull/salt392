Type not give for function parameter
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: 353) {  }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected type
   --> example.rs:1:21
    |
  1 | fn foo (x: bool, y: 353) {  }
    |                     ^^^ expected type
