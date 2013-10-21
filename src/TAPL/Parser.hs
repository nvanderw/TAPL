module TAPL.Parser where

import ClassyPrelude
import TAPL.Expr
import Text.Parsec hiding ((<|>))

padded :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
padded = between spaces spaces

-- |Identifiers
ident :: Stream s m Char => ParsecT s u m Expr
ident = do
    c <- letter 
    cs <- many alphaNum
    return . EIdent . pack $ c:cs

lambda :: Stream s m Char => ParsecT s u m Expr
lambda = do
    char '\\'
    (EIdent x) <- padded ident
    char '.'
    e <- between (char '[') (char ']') $ padded expr
    return $ ELambda x e

app :: Stream s m Char => ParsecT s u m Expr
app = between (char '(') (char ')') $ do
    es <- padded $ sepBy1 expr spaces
    if length es < 2
        then fail "Application with no arguments"
        else let (e:es') = es in return $ foldl' EApp e es'

expr :: Stream s m Char => ParsecT s u m Expr
expr = ident <|> lambda <|> app
