use codegen::{toolchain, CodeGen};
use parser::{Lexer, Parser, SourceFile, TokenKind};
use syntax::{Expander, Resolver, TypeChecker};

use anyhow::Result;
use clap::{Args, Parser as ClapParser, Subcommand};

use std::fs::File;
use std::io::{Cursor, Seek, SeekFrom, Write};
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
    Gen(GenArgs),
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

#[derive(Args, Debug)]
struct GenArgs {
    source: PathBuf,
    #[clap(short, long)]
    output: Option<PathBuf>,
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
            let mut module = parser.parse()?;

            if args.resolve {
                let mut resolver = Resolver::new();
                module = resolver.resolve(module)?;
            }

            if args.type_check {
                let mut type_checker = TypeChecker::new();
                let mut module = type_checker.type_check(module)?;

                let expander = Expander::new();
                module = expander.expand(module)?;

                println!("{:#?}", module);

            } else {
                println!("{:#?}", module);
            }
        }
        Command::Gen(args) => {
            let source_file = SourceFile::load(&args.source)?;
            let mut parser = Parser::new(&source_file);
            let mut module = parser.parse()?;

            let mut resolver = Resolver::new();
            module = resolver.resolve(module)?;

            let mut type_checker = TypeChecker::new();
            let mut module = type_checker.type_check(module)?;

            let expander = Expander::new();
            module = expander.expand(module)?;

            let mut cursor = Cursor::new(Vec::new());
            let mut codegen = CodeGen::new(&mut cursor);
            codegen.gen(&module)?;
            cursor.seek(SeekFrom::Start(0))?;
            let formatted = toolchain::go_fmt(&mut cursor)?;

            match &args.output {
                Some(output) => {
                    let mut file = File::create(output)?;
                    file.write_all(formatted.as_bytes())?;
                }
                None => {
                    println!("{}", formatted);
                }
            };

        }
    }

    Ok(())
}
