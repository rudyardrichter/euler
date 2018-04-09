extern crate euler;

use euler::prime::factorize;

fn main() -> () {
    let factors: Vec<(usize, usize)> = factorize(600851475143);
    let answer = &factors.last().unwrap().0;
    print!("{}\n", answer);
}
