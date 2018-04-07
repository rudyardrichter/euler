extern crate euler;

use euler::num::pow;

fn main() -> () {
    let square_of_sum: usize = pow((1..100).sum(), 2);
    let sum_of_square: usize = (1..100).map(|n| pow(n, 2)).sum();
    let answer = square_of_sum - sum_of_square;
    print!("{}\n", answer);
}
