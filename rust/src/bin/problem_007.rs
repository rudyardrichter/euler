extern crate euler;

use euler::prime::nth_prime;

fn main() -> () {
    let answer = nth_prime(10001);
    print!("{}\n", answer);
}
