use std::collections::HashSet;

pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    let mut multiples = HashSet::new();
    for factor in factors {
        let mut n = 1;
        while factor * n < limit {
            multiples.insert(factor * n);
            if *factor == 0 { break }
            n += 1;
        }
    }
    println!("{:?}", multiples);
    get_sum_of_list(&multiples)
}

pub fn get_sum_of_list(list: &HashSet<u32>) -> u32 {
    let mut sum = 0;
    for i in list {
        sum += i;
    }
    sum
}
