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
  >   if true { true } else true;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `{`
   --> example.rs:9:25
    |
  9 |   if true { true } else true;
    |                         ^^^^ expected `{`
