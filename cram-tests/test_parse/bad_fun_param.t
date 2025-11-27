Missing parameter identifier
  $ cat > example.rs <<EOF
  > fn foo (: bool) {  }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected identifier or `)`
   --> example.rs:1:9
    |
  1 | fn foo (: bool) {  }
    |         ^ expected identifier or `)`
