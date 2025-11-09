Missing second tuple type
  $ cat > example.rs <<EOF
  > fn main () {
  >     // empty
  > }
  > fn bar (x: (i32,i32,, y: i32) -> i32 { x }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected type or `)`
   --> example.rs:4:21
    |
  4 | fn bar (x: (i32,i32,, y: i32) -> i32 { x }
    |                     ^ expected type or `)`
