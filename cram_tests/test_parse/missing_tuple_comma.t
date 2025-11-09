Missing second tuple type
  $ cat > example.rs <<EOF
  > fn main () {
  >     // empty
  > }
  > fn bar (x: (i32)) -> i32 { x }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `,`
   --> example.rs:4:16
    |
  4 | fn bar (x: (i32)) -> i32 { x }
    |                ^ expected `,`
