#[allow(unused_variables, unused_assignments, unused_mut)]

fn main() {
    let mut x = 2;
    let x2 = 3;
    let mut y = &x;
    let z = &*y;
    y = &x2;
    x = 3;
    // let mut x = 2;
    // let mut y = &mut x;
    // let mut z = &mut y;
    // let mut a = &*z;
    // let mut x_ = 3;
    // let mut y_ = &mut x_;
    // z = &mut y_;
    // let mut x__ = 4;
    // y = &mut x__;
    // let b = **a;

    // let mut x = 1;
    // let mut y = &x;
    // let mut z = &*y;
    // let mut q = 3;
    // y = &q;
    // let r = &mut x;
    // let a = *z;
    // let mut a = 0;
    // let mut b = 1;
    // let mut c = 2;
    // let mut d = &mut a;
    // let mut e = &mut *d;
    // d = &mut b;
    // *d = 3;
    // // e = &c;
    // // let f = &mut a;
    // let g = *e;

    // let mut a = 2;
    // let mut b = 3;
    // let mut q = 4;
    // let mut c = &mut a;
    // let mut d = &*c;
    // c = &mut b;
    // d = &q;
    // let e = &mut a;
    // *c = 5;
    // *e = 6;
    // let f = *d;
}
