// use xshell::{cmd, Shell};

fn main() -> anyhow::Result<()> {
    let arg = std::env::args().nth(1).unwrap();
    for i in 0..10 {
        println!("{arg}: {i}");
        std::thread::sleep(std::time::Duration::from_secs(1))
    }
    Ok(())
}
