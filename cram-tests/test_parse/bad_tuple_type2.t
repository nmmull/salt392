Type not given for tuple type
  $ cat > example.rs <<EOF
  > fn foo (x: bool, y: i32, z: (i32, true,)) {  }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected type
   --> example.rs:1:35
    |
  1 | fn foo (x: bool, y: i32, z: (i32, true,)) {  }
    |                                   ^^^^ expected type
