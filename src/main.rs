mod error;

fn main() {
    let e = error::Err {
        reason: error::ERR_ASSERT,
        message: "test".to_string(),
    };
    println!("Hello, world! {}", e.error());
}
