#![feature(deref_patterns, if_let_guard)]
use std::{env::args, process::exit};

use codegen::Codegen;
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

mod ast;
mod symbols;
mod codegen;
lrlex_mod!("klein.l");
lrpar_mod!("klein.y");

fn main() {
    let Some(filename) = args().nth(1) else {
        println!("Usage: kleinc <file>");
        exit(1);
    };
    let path = std::path::Path::new(&filename);
    let asm_path = path.with_extension("s");
    let file = std::fs::read_to_string(path).unwrap(); 
    let lexerdef = klein_l::lexerdef();
    let lexer = lexerdef.lexer(&file);
    let (res, errs) = klein_y::parse(&lexer);
    for e in errs {
        println!("{}", e.pp(&lexer, &klein_y::token_epp));
    }
    match res {
        Some(Ok(program)) => {
            if !program.name_analysis(&file) {
                println!("Processing failed during name analysis");
                return;
            } 
            let Ok(ctx) = program.type_checking(&file) else {
                program.print(&file);
                println!("Processing failed during type checking");
                return;
            };
            program.compute_offsets();
            program.print(&file);
            let out_stream = std::fs::File::create(asm_path)
                .unwrap();
            let mut codegen = Codegen::new(Box::new(out_stream), &file, ctx.type_cache);
            program.codegen(&mut codegen);
        } 
        _ => eprintln!("Unable to parse program"),
    }
}
