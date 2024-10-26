use parser::{Lexer, Parser, SourceFile, TokenKind};

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
    Lex(SourceArgs),
    Parse(SourceArgs),
}

#[derive(Args, Debug)]
struct SourceArgs {
    source: PathBuf,
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
            let block = parser.parse()?;
            println!("{:#?}", block);
        }
    }

    Ok(())
}
