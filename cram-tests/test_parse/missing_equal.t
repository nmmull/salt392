Missing equal sign in let statement
  $ cat > example.rs <<EOF
  > fn foo (x: i32) {
  >     let x true;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `=`
   --> example.rs:2:11
    |
  2 |     let x true;
    |           ^^^^ expected `=`
