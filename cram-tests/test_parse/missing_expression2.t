Empty expression
  $ cat > example.rs <<EOF
  > fn foo(x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar(x: bool, y: &mut i32) -> bool {
  >     false
  > }
  > fn baz(okay: (bool,bool,bool)) {
  >   let x = 3;
  >   ;
  >   let y = 2;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected expression or statement
   --> example.rs:9:3
    |
  9 |   ;
    |   ^ expected expression or statement
