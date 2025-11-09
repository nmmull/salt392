Empty expression
  $ cat > example.rs <<EOF
  > fn bar(x: bool, y: &mut i32) -> bool {
  >     false;
  >     ***x = &**true;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected place expression
   --> example.rs:3:15
    |
  3 |     ***x = &**true;
    |               ^^^^ expected place expression
