Empty expression
  $ cat > example.rs <<EOF
  > fn bar(x: bool, y: &mut i32) -> bool {
  >     false;
  >     ***true = 2;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected place expression
   --> example.rs:3:8
    |
  3 |     ***true = 2;
    |        ^^^^ expected place expression
