Trailing expression
  $ cat > example.rs <<EOF
  > fn foo (x: i32) {
  >     let .3 = true;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected identifer or `mut`
   --> example.rs:2:9
    |
  2 |     let .3 = true;
    |         ^ expected identifer or `mut`
