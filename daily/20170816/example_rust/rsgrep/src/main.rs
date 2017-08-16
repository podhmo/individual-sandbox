use std::fs::File;
use std::io::{BufReader, BufRead};
use std::env;

fn usage(){
    println!("rsgrep <pattern> <filename>")
}

fn main() {
    let filename = match env::args().nth(2) {
        Some(filename) => filename,
        None => {
            usage();
            return;
        }
    };

    let file = match File::open(&filename) {
        Ok(file) => file,
        Err(e) => {
            println!("file {}:{}", filename, e);
            return;
        }
    };

    let input = BufReader::new(file);
    for line in input.lines() {
        let line = match line {
            Ok(line) => line,
            Err(e) => {
                println!("line {}:{}", filename, e);
                return;
            }
        };
        println!("{}", line)
    }
}
