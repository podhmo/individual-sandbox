use std::fs::File;
use std::io::{BufReader, BufRead};
use std::env;
extern crate regex;
use regex::Regex;


fn usage(){
    println!("rsgrep <pattern> <filename>")
}

fn main() {
    let pattern = match env::args().nth(1) {
        Some(pattern) => pattern,
        None => {
            usage();
            return;
        }
    };

    let rx = match Regex::new(&pattern) {
        Ok(reg) => reg,
        Err(e) => {
            println!("regex {}", e);
            return;
        }
    };

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
        if rx.is_match(&line) {
            println!("{}", line)
        }
    }
}
