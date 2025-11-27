Missing closing parenthesis
  $ cat > example.rs <<EOF
  > fn foo(x: bool, y: i32, z: (bool, i32)) -> bool {
  >     true
  > }
  > fn bar(x: bool, y: &mut i32) -> bool {
  >     false
  > }
  > fn baz(okay: (bool,bool,bool))
  >   let x = 3;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `->` or `{`
   --> example.rs:8:3
    |
  8 |   let x = 3;
    |   ^^^ expected `->` or `{`
