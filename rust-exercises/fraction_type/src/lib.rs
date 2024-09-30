use std::ops;

#[derive(Debug, PartialEq, Eq)]
pub struct Fraction(pub i32, pub i32);

impl Fraction {
    pub fn divide(self, rhs: Fraction) -> Fraction {
        ops::Div::div(self, rhs)
    }

    pub fn add(self, rhs: Fraction) -> Fraction {
        ops::Add::add(self, rhs)
    }

    pub fn sub(self, rhs: Fraction) -> Fraction {
        ops::Sub::sub(self, rhs)
    }

    pub fn mul(self, rhs: Fraction) -> Fraction {
        ops::Mul::mul(self, rhs)
    }
}

impl ops::Add for Fraction {
    type Output = Fraction;

    fn add(self, rhs: Fraction) -> Fraction {
        let Fraction(n1, d1) = self;
        let Fraction(n2, d2) = rhs;

        let numerator = n1 * d2 + n2 * d1;
        let denominator = d1 * d2;

        simplify(numerator, denominator)
    }
}

impl ops::Sub for Fraction {
    type Output = Fraction;

    fn sub(self, rhs: Fraction) -> Fraction {
        let Fraction(n1, d1) = self;
        let Fraction(n2, d2) = rhs;

        let numerator = n1 * d2 - n2 * d1;
        let denominator = d1 * d2;

        simplify(numerator, denominator)
    }
}

impl ops::Mul for Fraction {
    type Output = Fraction;

    fn mul(self, rhs: Fraction) -> Fraction {
        let Fraction(n1, d1) = self;
        let Fraction(n2, d2) = rhs;

        let numerator = n1 * n2;
        let denominator = d1 * d2;

        simplify(numerator, denominator)
    }
}

impl ops::Div for Fraction {
    type Output = Fraction;

    fn div(self, rhs: Fraction) -> Fraction {
        let Fraction(n1, d1) = self;
        let Fraction(n2, d2) = rhs;

        let numerator = n1 * d2;
        let denominator = d1 * n2;

        simplify(numerator, denominator)
    }
}

/// Calculate the Highest common factor between 2 numbers
fn hcf(a: i32, b: i32) -> i32 {
    if b == 0 { a } else { hcf(b, a % b) }
}

fn simplify(n: i32, d: i32) -> Fraction {
    let h = hcf(n, d);
    Fraction(n/h, d/h)
}