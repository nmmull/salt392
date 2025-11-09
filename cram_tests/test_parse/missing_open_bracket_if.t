Missing opening bracket for if expression
  $ cat > example.rs <<EOF
  > fn foo(x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar(x: bool, y: &mut i32) -> bool {
  >     false
  > }
  > fn baz(okay: (bool,bool,bool)) -> () {
  >   let x = 3;
  >   if true + 3 ;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `{`
   --> example.rs:9:15
    |
  9 |   if true + 3 ;
    |               ^ expected `{`
