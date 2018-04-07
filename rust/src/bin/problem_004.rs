extern crate euler;

use euler::misc::is_palindrome;

fn main() -> () {
    let mut answer = 0;
    for i in 0..1000 {
        for j in 0..1000 {
            let product = i * j;
            if product > answer && is_palindrome(product) {
                answer = product;
            }
        }
    }
    print!("{}\n", answer);
}
