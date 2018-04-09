extern crate euler;

use euler::num::TriangleNumberIterator;
use euler::prime::factorize;

fn n_divisors(n: usize) -> usize {
    factorize(n).iter().map(|&(_, exponent)| exponent + 1).product()
}

fn main() -> () {
    let t_iter = TriangleNumberIterator::new();
    let answer = t_iter.filter(|&n| n_divisors(n) > 500).take(1).next().unwrap();
    print!("{}\n", answer);
}
