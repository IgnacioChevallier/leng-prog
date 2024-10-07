use std::collections::HashMap;

pub fn word_count(phrase: &str) -> HashMap<String, u32> {
    let words: Vec<String> = get_words_lowercased(phrase);
    let mut word_count = HashMap::new();

    for word in words {
        let count = word_count.entry(word).or_insert(0);
        *count += 1;
    }

    word_count
}

pub fn get_words_lowercased(phrase: &str) -> Vec<String> {
    phrase.split_whitespace().map(|word| word.to_lowercase()).collect()
}