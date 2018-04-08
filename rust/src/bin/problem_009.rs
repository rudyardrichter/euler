fn solve() -> Option<usize> {
    for c in 1..500 {
        for b in 1..c {
            let a = 1000 - b - c;
            if a*a + b*b == c*c {
                return Some(a * b * c);
            }
        }
    }
    None
}

fn main() -> () {
    let answer = solve().unwrap();
    print!("{}\n", answer);
}
