use crate::parse::ParseArena;
use crate::parse::ParseError;
use crate::tokenize::KeywordKind;
use crate::tokenize::PunctKind;
use crate::tokenize::TokenKind;
use crate::tokenize::TokenList;
use crate::types::StructDecl;
use crate::types::Type;
use crate::types::TypeNode;

use std::collections::HashMap;
use std::collections::VecDeque;

impl ParseArena {
    pub fn parse_decl_spec(&mut self, tok_seq: TokenList) -> Result<(TokenList, Type), ParseError> {
        let mut scope = VecDeque::new();
        self.parse_decl_spec_with_scope(tok_seq, &mut scope)
    }

    pub fn parse_decl_spec_with_scope(
        &mut self,
        mut tok_seq: TokenList,
        scope: &mut VecDeque<HashMap<String, Type>>,
    ) -> Result<(TokenList, Type), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Int).is_some() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, Type::new(TypeNode::Int)))
        } else if tok_seq.expect_keyword(KeywordKind::Char).is_some() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, Type::new(TypeNode::Char)))
        } else if tok_seq.expect_keyword(KeywordKind::Struct).is_some() {
            tok_seq = tok_seq.next();

            let TokenKind::Ident(ref tag) = tok_seq.get_token() else {return Err(ParseError::SyntaxError(tok_seq));};
            tok_seq = tok_seq.next();

            if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
                tok_seq = tok_seq.next();
                scope.push_back(HashMap::new());

                unimplemented!()
            } else {
                let ty = Type::new(TypeNode::Struct(StructDecl {
                    id: self.struct_id,
                    tag: tag.clone(),
                    members: None,
                }));
                self.struct_id += 1;

                Ok((tok_seq, ty))
            }
        } else {
            Err(ParseError::SyntaxError(tok_seq))
        }
    }
}
