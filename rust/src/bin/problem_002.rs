extern crate euler;

use euler::fibonacci_iterator::Fibonacci;

fn main() -> () {
    let answer: usize =
        Fibonacci::new()
        .take_while(|n| *n < 4000000)
        .filter(|n| n % 2 == 0)
        .sum();
    print!("{}\n", answer)
}
