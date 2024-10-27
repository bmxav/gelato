use parser::{Lexer, Parser, SourceFile, TokenKind};
use syntax::{Resolver, TypeChecker};

use anyhow::Result;
use clap::{Args, Parser as ClapParser, Subcommand};

use std::path::PathBuf;

#[derive(ClapParser, Debug)]
struct Gelato {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Lex(LexArgs),
    Parse(ParseArgs),
}

#[derive(Args, Debug)]
struct LexArgs {
    source: PathBuf,
}

#[derive(Args, Debug)]
struct ParseArgs {
    source: PathBuf,
    #[clap(long, action)]
    resolve: bool,
    #[clap(long, action)]
    type_check: bool,
}

fn main() -> Result<()> {
    let gelato = Gelato::parse();

    match &gelato.command {
        Command::Lex(args) => {
            let content = std::fs::read_to_string(&args.source)?;
            let mut lexer = Lexer::new(&content);
            loop {
                let token = lexer.next_token();
                println!("{:?}", token);
                if token.kind == TokenKind::Eof {
                    break;
                }
            }
        }
        Command::Parse(args) => {
            let source_file = SourceFile::load(&args.source)?;
            let mut parser = Parser::new(&source_file);
            let mut block = parser.parse()?;

            if args.resolve {
                let mut resolver = Resolver::new();
                block = resolver.resolve(block)?;
            }

            if args.type_check {
                let mut type_checker = TypeChecker::new();
                let typed_block = type_checker.type_check(block)?;
                println!("{:#?}", typed_block);
            } else {
                println!("{:#?}", block);
            }
        }
    }

    Ok(())
}
