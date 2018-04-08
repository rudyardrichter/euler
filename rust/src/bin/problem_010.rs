extern crate euler;

use euler::prime::primes;

fn main() -> () {
    let answer: usize = primes().take_while(|&p| p < 2_000_000).sum();
    print!("{}\n", answer);
}
