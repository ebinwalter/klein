use lrlex::CTLexerBuilder;

fn main() {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(cfgrammar::yacc::YaccKind::Grmtools)
                .grammar_in_src_dir("klein.y")
                .unwrap()
        })
        .lexer_in_src_dir("klein.l")
        .unwrap()
        .build()
        .unwrap();
}
