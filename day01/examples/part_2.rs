fn main() {
    let path = format!("{}/input.txt", env!("CARGO_MANIFEST_DIR"));
    let file = std::fs::File::open(path).unwrap();
    let final_value = aoc24day01::part_2::calculate_final_similarity_score(file).unwrap();
    println!("Got final value {final_value}")
}
