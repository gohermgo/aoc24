fn main() {
    let path = format!("{}/input.txt", env!("CARGO_MANIFEST_DIR"));
    let file = std::fs::File::open(path).unwrap();
    let final_value = day01::part01::calculate_final_distance(file).unwrap();
    println!("Got final value {final_value}")
}
