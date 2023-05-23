use crate::error::ParseError;
use crate::parse::ParseArena;
use crate::tokenize::PunctKind;
use crate::tokenize::TokenKind;
use crate::tokenize::TokenList;
use crate::types::Type;

use std::rc::Rc;

#[derive(Clone)]
enum DeclaratorNest {
    Name(String),
    Nest(Rc<Declarator>),
}

#[derive(Clone)]
enum DeclaratorSuffix {
    None,
    Array(u32),
    Function(Vec<(Type, Declarator)>),
}

#[derive(Clone)]
pub struct Declarator {
    pointer_cnt: u32,
    nest: DeclaratorNest,
    suffix: DeclaratorSuffix,
}

impl Declarator {
    pub fn get_name(&self) -> String {
        match self.nest {
            DeclaratorNest::Name(ref name) => name.clone(),
            DeclaratorNest::Nest(ref declarator) => (*declarator).get_name(),
        }
    }

    pub fn get_type(&self, decl_spec_type: Type) -> Type {
        let mut ret_type = decl_spec_type;
        for _ in 0..self.pointer_cnt {
            ret_type = Type::new_ptr_type(ret_type);
        }

        ret_type = match self.suffix {
            DeclaratorSuffix::None => ret_type,
            DeclaratorSuffix::Array(len) => Type::new_array_type(ret_type, len),
            DeclaratorSuffix::Function(ref arg) => {
                let mut arg_type = Vec::new();
                for (ref decl_spec_type, ref declarator) in arg.iter() {
                    arg_type.push(declarator.get_type(decl_spec_type.clone()));
                }
                Type::new_fn_type(ret_type, arg_type)
            }
        };

        match self.nest {
            DeclaratorNest::Name(_) => ret_type,
            DeclaratorNest::Nest(ref declarator) => (*declarator).get_type(ret_type),
        }
    }

    pub fn get_args(&self) -> Vec<(Type, Declarator)> {
        match self.suffix {
            DeclaratorSuffix::Function(ref args) => args.to_vec(),
            _ => panic!(),
        }
    }
}

impl ParseArena {
    pub fn parse_declarator(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Declarator), ParseError> {
        let mut pointer_cnt = 0;
        while tok_seq.expect_punct(PunctKind::Asterisk).is_some() {
            pointer_cnt += 1;
            tok_seq = tok_seq.next();
        }

        let nest = if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq.next();

            let declarator;
            (tok_seq, declarator) = self.parse_declarator(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            DeclaratorNest::Nest(Rc::new(declarator))
        } else if let TokenKind::Ident(name) = tok_seq.get_token() {
            tok_seq = tok_seq.next();
            DeclaratorNest::Name(name)
        } else {
            return Err(ParseError::SyntaxError(tok_seq));
        };

        let suffix = if tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
            tok_seq = tok_seq.next();

            let len = match tok_seq.get_token() {
                TokenKind::Number(len) => len,
                _ => return Err(ParseError::SyntaxError(tok_seq)),
            };
            tok_seq = tok_seq.next();

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseSquareBracket)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            DeclaratorSuffix::Array(len as u32)
        } else if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq.next();

            let mut args = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseParenthesis).is_none() {
                let decl_spec_type;
                (tok_seq, decl_spec_type) = self.parse_decl_spec(tok_seq)?;

                let declarator;
                (tok_seq, declarator) = self.parse_declarator(tok_seq)?;

                args.push((decl_spec_type, declarator));

                if tok_seq.expect_punct(PunctKind::Comma).is_some() {
                    tok_seq = tok_seq.next();
                }
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            DeclaratorSuffix::Function(args)
        } else {
            DeclaratorSuffix::None
        };

        Ok((
            tok_seq,
            Declarator {
                pointer_cnt,
                nest,
                suffix,
            },
        ))
    }
}
