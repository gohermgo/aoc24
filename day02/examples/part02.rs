fn main() {
    let path = format!("{}/input.txt", env!("CARGO_MANIFEST_DIR"));
    let file = std::fs::File::open(path).unwrap();
    let final_value = day02::part02::calculate_safe_adjusted_report_count(file);
    println!("Got final value {final_value}")
}
