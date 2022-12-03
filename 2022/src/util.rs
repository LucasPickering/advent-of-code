/// Split a string on spaces
pub fn words(input: &str) -> Vec<&str> {
    input.split(' ').collect::<Vec<_>>()
}
