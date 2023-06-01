use crate::parse::ParseArena;
use crate::parse::ParseError;
use crate::tokenize::KeywordKind;
use crate::tokenize::TokenList;
use crate::types::Type;
use crate::types::TypeNode;

#[derive(Clone)]
pub enum DeclSpec {
    Int,
    Char,
}

impl DeclSpec {
    pub fn get_type(&self) -> Type {
        match self {
            DeclSpec::Int => Type::new(TypeNode::Int),
            DeclSpec::Char => Type::new(TypeNode::Char),
        }
    }
}

impl ParseArena {
    pub fn parse_decl_spec(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, DeclSpec), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Int).is_some() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, DeclSpec::Int))
        } else if tok_seq.expect_keyword(KeywordKind::Char).is_some() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, DeclSpec::Char))
        } else {
            Err(ParseError::SyntaxError(tok_seq))
        }
    }
}
