use std::io::{stdin, BufReader};

mod risp;

fn main() {
    let mut risp = risp::Risp::new(BufReader::new(stdin()));

    loop {
        risp.input();
        let r = risp.run();
        risp.print(r);
    }
}
