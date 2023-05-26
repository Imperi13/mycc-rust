use crate::tokenize::TokenList;

use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("failed to parse\nTokenList:\n{0:?}\n")]
    SyntaxError(TokenList),

    #[error("failed to analyze\nTokenList:\n{0:?}\n")]
    SemanticError(TokenList),
}
