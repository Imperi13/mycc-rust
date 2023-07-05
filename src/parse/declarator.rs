use crate::error::ParseError;
use crate::parse::ParseArena;
use crate::tokenize::KeywordKind;
use crate::tokenize::PunctKind;
use crate::tokenize::TokenKind;
use crate::tokenize::TokenList;
use crate::types::Type;

#[derive(Clone)]
enum DeclaratorNest {
    Name(String),
    Nest(Box<Declarator>),
}

#[derive(Clone)]
enum DeclaratorSuffix {
    None,
    Array(Vec<u32>),
    Function(Option<Vec<(Type, Declarator)>>),
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
            DeclaratorSuffix::Array(ref vec) => {
                let mut ty = ret_type;
                for len in vec.iter().rev() {
                    ty = Type::new_array_type(ty, len.clone())
                }
                ty
            }
            DeclaratorSuffix::Function(ref arg) => {
                let arg_type = if arg.is_some() {
                    let mut arg_type = Vec::new();
                    for (ref decl_spec_type, ref declarator) in arg.clone().unwrap().iter() {
                        let ty = declarator.get_type(decl_spec_type.clone());
                        let ty = if ty.is_array_type() {
                            Type::new_ptr_type(ty.get_array_to().unwrap())
                        } else {
                            ty
                        };

                        arg_type.push(ty);
                    }
                    Some(arg_type)
                } else {
                    None
                };

                Type::new_fn_type(ret_type, arg_type)
            }
        };

        match self.nest {
            DeclaratorNest::Name(_) => ret_type,
            DeclaratorNest::Nest(ref declarator) => (*declarator).get_type(ret_type),
        }
    }

    pub fn get_args(&self) -> Option<Vec<(Type, Declarator)>> {
        match self.suffix {
            DeclaratorSuffix::Function(ref args) => args.clone(),
            _ => panic!(),
        }
    }
}

impl ParseArena {
    pub fn parse_declarator(
        &mut self,
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

            DeclaratorNest::Nest(Box::new(declarator))
        } else if let TokenKind::Ident(name) = tok_seq.get_token() {
            tok_seq = tok_seq.next();
            DeclaratorNest::Name(name)
        } else {
            return Err(ParseError::SyntaxError(tok_seq));
        };

        let suffix = if tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
            let mut lens = Vec::new();

            while tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
                tok_seq = tok_seq.next();

                let len = match tok_seq.get_token() {
                    TokenKind::Number(len) => len,
                    _ => return Err(ParseError::SyntaxError(tok_seq)),
                };
                tok_seq = tok_seq.next();

                tok_seq = tok_seq
                    .expect_punct(PunctKind::CloseSquareBracket)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                lens.push(len as u32);
            }

            DeclaratorSuffix::Array(lens)
        } else if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq.next();

            let args = if tok_seq.expect_punct(PunctKind::CloseParenthesis).is_some() {
                tok_seq = tok_seq.next();
                None
            } else if tok_seq.equal_keyword(KeywordKind::Void)
                && tok_seq.next().equal_punct(PunctKind::CloseParenthesis)
            {
                tok_seq = tok_seq.next().next();
                Some(Vec::new())
            } else {
                let mut args = Vec::new();

                while tok_seq.expect_punct(PunctKind::CloseParenthesis).is_none() {
                    let decl_spec;
                    (tok_seq, decl_spec) = self.parse_decl_spec(tok_seq)?;

                    let declarator;
                    (tok_seq, declarator) = self.parse_declarator(tok_seq)?;

                    args.push((decl_spec, declarator));

                    if tok_seq.expect_punct(PunctKind::Comma).is_some() {
                        tok_seq = tok_seq.next();
                    }
                }

                tok_seq = tok_seq
                    .expect_punct(PunctKind::CloseParenthesis)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                Some(args)
            };

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
