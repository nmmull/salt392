Trailing expression
  $ cat > example.rs <<EOF
  > fn foo (x: i32) {
  >     let y = !2 false || true;
  > }
  > fn main () {
  >   let x = foo(0);
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: unexpected token within expression
   --> example.rs:2:16
    |
  2 |     let y = !2 false || true;
    |                ^^^^^ unexpected token within expression
