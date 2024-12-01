fn main() {
    let path = format!("{}/input.txt", env!("CARGO_MANIFEST_DIR"));
    let file = std::fs::File::open(path).unwrap();
    let final_value = aoc24day01::part_1::calculate_final_distance(file).unwrap();
    println!("Got final value {final_value}")
}
