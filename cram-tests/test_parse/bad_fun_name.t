Non-identifier as function name
  $ cat > example.rs <<EOF
  > fn f () { 2 }
  >   fn 3 + 3 () {
  >      let x = true;
  >      if x { x }
  >   }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected identifier
   --> example.rs:2:6
    |
  2 |   fn 3 + 3 () {
    |      ^ expected identifier
