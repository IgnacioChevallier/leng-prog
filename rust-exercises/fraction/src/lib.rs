type Fraction = (i32, i32);

/// Add 2 fractions
pub fn add((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    return simplify(n1 * d2 + n2 * d1, d1 * d2);
}

/// Subtract 2 fractions
pub fn sub((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    return simplify(n1 * d2 - n2 * d1, d1 * d2);
}

/// Multiply 2 fractions
pub fn mul((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    return (n1 * n2, d1 * d2);
}

/// Divide 2 fractions
pub fn divide((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    return simplify(n1 * d2, d1 * n2);
}

/// Calculate the Highest common factor between 2 numbers
pub fn hcf(a: i32, b: i32) -> i32 {
    let a = a;
    let b = b;
    if a > b {
        return find_hcf(a, b, b)
    } else {
        return find_hcf(b, a, a)
    }
}

fn find_hcf(a: i32, b: i32, n: i32) -> i32 {
    if a % n == 0 && b % n == 0 {
        return n
    } else {
        return find_hcf(a, b, n - 1)
    }
}

/// Create a fraction simplifying with the arguments simplified by the `hcf`
pub fn simplify(n: i32, d: i32) -> Fraction {
    let hcf = hcf(n, d);
    return (n / hcf, d / hcf);
}
