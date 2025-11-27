Missing colon
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z bool) {  }
  > fn main () {
  >    foo();
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `:`
   --> example.rs:1:28
    |
  1 | fn foo (x: bool, y: i32, z bool) {  }
    |                            ^^^^ expected `:`
