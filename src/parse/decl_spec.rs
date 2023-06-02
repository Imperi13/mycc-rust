use crate::parse::ParseArena;
use crate::parse::ParseError;
use crate::tokenize::KeywordKind;
use crate::tokenize::TokenList;
use crate::types::Type;
use crate::types::TypeNode;

use std::collections::HashMap;

impl ParseArena {
    pub fn parse_decl_spec(&self, mut tok_seq: TokenList) -> Result<(TokenList, Type), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Int).is_some() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, Type::new(TypeNode::Int)))
        } else if tok_seq.expect_keyword(KeywordKind::Char).is_some() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, Type::new(TypeNode::Char)))
        } else if tok_seq.expect_keyword(KeywordKind::Struct).is_some() {
            let mut scope = Vec::new();
            self.parse_struct_spec(tok_seq, &mut scope)
        } else {
            Err(ParseError::SyntaxError(tok_seq))
        }
    }

    pub fn parse_struct_spec(
        &self,
        mut tok_seq: TokenList,
        scope: &mut Vec<HashMap<String, Type>>,
    ) -> Result<(TokenList, Type), ParseError> {
        unimplemented!()
    }
}
