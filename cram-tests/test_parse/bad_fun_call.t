Missing closing parenthesis for function call
  $ cat > example.rs <<EOF
  > fn foo() {
  >     bar(2 + 3, true, ;
  > }
  > fn main () {
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected expression or `)`
   --> example.rs:2:22
    |
  2 |     bar(2 + 3, true, ;
    |                      ^ expected expression or `)`
