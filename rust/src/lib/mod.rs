extern crate primal;

pub mod fibonacci_iterator;

pub mod misc {
    use std::fmt::Display;

    pub fn is_palindrome<T: Display>(x: T) -> bool {
        let chars_vec: Vec<char> = x.to_string().chars().collect();
        let chars_vec_rev: Vec<char> = chars_vec.iter().cloned().rev().collect();
        chars_vec == chars_vec_rev
    }
}

pub mod num {
    /// Return `n` to the `exp` power.
    pub fn pow(n: usize, exp: usize) -> usize {
        let mut e = exp;
        let mut result_a = n;
        let mut result_b = 1;
        while e > 1 {
            if e % 2 == 0 {
                result_a *= result_a;
                e = e / 2;
            } else {
                result_b *= result_a;
                result_a *= result_a;
                e = (e - 1) / 2;
            }
        }
        result_a * result_b
    }

    /// Return the square root of `n`, rounded up to the nearest integer.
    pub fn sqrt(n: usize) -> usize {
        (n as f64 + 1.0).sqrt() as usize
    }

    /// Use the Euclidean algorithm to compute GCD.
    pub fn gcd(m: usize, n: usize) -> usize {
        let mut a = m;
        let mut b = n;
        while b != 0 {
            let b_prv = b;
            b = a % b_prv;
            a = b_prv;
        }
        a
    }

    pub fn lcm(m: usize, n: usize) -> usize {
        (m * n) / gcd(m, n)
    }
}

pub mod prime {
    use primal;

    pub fn nth_prime(n: usize) -> usize {
        primal::StreamingSieve::nth_prime(n)
    }

    pub mod factor {
        use num::sqrt;
        use primal;

        pub fn factorize(n: usize) -> Vec<(usize, usize)> {
            primal::Sieve::new(sqrt(n)).factor(n).expect("number too large to factor")
        }
    }
}
