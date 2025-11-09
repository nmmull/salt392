Type not given for tuple type
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (true, i32)) {  }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected type
   --> example.rs:1:30
    |
  1 | fn foo (x: bool, y: i32, z: (true, i32)) {  }
    |                              ^^^^ expected type
