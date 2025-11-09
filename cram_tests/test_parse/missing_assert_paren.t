Missing opening parenthesis for assert
  $ cat > example.rs <<EOF
  > fn main () {
  >   let foo = Box::new(2);
  >     let bar = assert! *foo == 2;
  > }
  > EOF
  $ ./parse_test.exe example.rs
  error: expected `(`
   --> example.rs:3:23
    |
  3 |     let bar = assert! *foo == 2;
    |                       ^ expected `(`
