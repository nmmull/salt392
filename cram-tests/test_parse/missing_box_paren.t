Missing opening parenthesis for box init
  $ cat > example.rs <<EOF
  > fn main () {
  >   let foo = Box::new  2 );
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `(`
   --> example.rs:2:23
    |
  2 |   let foo = Box::new  2 );
    |                       ^ expected `(`
