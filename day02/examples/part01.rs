fn main() {
    let path = format!("{}/input.txt", env!("CARGO_MANIFEST_DIR"));
    let file = std::fs::File::open(path).unwrap();
    let final_value = day02::part01::calculate_safe_report_count(file);
    println!("Got final value {final_value}")
}
