Missing second parameter identifier
  $ cat > example.rs <<EOF
  > // a function I wrote
  > fn foo (x: bool,3) {  }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected identifier or `)`
   --> example.rs:2:17
    |
  2 | fn foo (x: bool,3) {  }
    |                 ^ expected identifier or `)`
