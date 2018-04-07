extern crate euler;

use euler::num::lcm;

fn main() -> () {
    let answer = (1..20).fold(1, lcm);
    print!("{}\n", answer);
}
