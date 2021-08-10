{-# LANGUAGE OverloadedStrings #-}

module Machinations.Formulas(parseSF,parseRF,isRFPercentage,parseF) where
import Machinations.Types
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- * ResourceFormula

rInteger :: Parser ResourceFormula
rInteger = RFConstant <$> lexeme L.decimal

rExpr :: Parser ResourceFormula
rExpr = makeExprParser rTerm rOperatorTable
  where rTerm :: Parser ResourceFormula
        rTerm = choice [ parens rExpr
                       , rInteger
                       ]

mkRandomDie p = RFDice (RFConstant 1) p
mkRandomDie' n = RFDice n (RFConstant 1)
mkRandomDice n p = RFDice n p

rOperatorTable :: [[Operator Parser ResourceFormula]]
rOperatorTable =
  [ [ postfix "%" RFPercentage
    ]
  , [ prefix "-" RFNegation
    , prefix "+" id
    , prefix "D" mkRandomDie
    , prefix "d" mkRandomDie
    ]
  , [ binary "D" mkRandomDice
    , binary "d" mkRandomDice
    ]
  , [ binary "*" RFMultiply
    , binary "/" RFDivide
    ]
  , [ prefix ">"  (RFCondition CGt)
    , prefix "<"  (RFCondition CLt)
    , prefix "==" (RFCondition CEqual)
    , prefix "!=" (RFCondition CNotEqual)
    , prefix ">=" (RFCondition CGtEq)
    , prefix "<=" (RFCondition CLtEq)
    ]
  , [ binary "+" RFAdd
    , binary "-" RFSubtract
    ]
  ]
  where binary :: Text -> (ResourceFormula -> ResourceFormula -> ResourceFormula) -> Operator Parser ResourceFormula
        binary  name f = InfixL  (f <$ symbol name)
        prefix, postfix :: Text -> (ResourceFormula -> ResourceFormula) -> Operator Parser ResourceFormula
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)

parseRF "" = pure $ RFConstant 0 -- Machinations allows empty edges :(
parseRF "all" = Just RFAll
parseRF s = parseMaybe (rExpr <* eof) s

isRFPercentage (RFPercentage p) = True
isRFPercentage _ = False

-- * StateFormula

sVariable :: Parser StateFormula
sVariable = SFVariable . T.pack <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

sInteger :: Parser StateFormula
sInteger = SFConstant <$> lexeme L.decimal

sExpr :: Parser StateFormula
sExpr = makeExprParser sTerm sOperatorTable
  where sTerm :: Parser StateFormula
        sTerm = choice [ parens sExpr
                       , sVariable
                       , sInteger
                       ]

sOperatorTable :: [[Operator Parser StateFormula]]
sOperatorTable =
  [ [ postfix "%" SFPercentage
    ]
  , [ postfix "i" SFInterval
    ]
  , [ prefix "-" SFSub
    , prefix "+" SFAdd
    , prefix "*" SFSub
    , prefix "/" SFAdd
    , prefix "=" SFOverwrite
    ]
  , [ prefix ">"  (SFCondition CGt)
    , prefix "<"  (SFCondition CLt)
    , prefix "==" (SFCondition CEqual)
    , prefix "!=" (SFCondition CNotEqual)
    , prefix ">=" (SFCondition CGtEq)
    , prefix "<=" (SFCondition CLtEq)
    ]
  , [ binary ".." SFRange
    ]
  ]
  where binary :: Text -> (StateFormula -> StateFormula -> StateFormula) -> Operator Parser StateFormula
        binary  name f = InfixL  (f <$ symbol name)
        prefix, postfix :: Text -> (StateFormula -> StateFormula) -> Operator Parser StateFormula
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)

parseSF "*" = Just SFTrigger
parseSF "!" = Just SFReverseTrigger
parseSF s = parseMaybe (sExpr <* eof) s

-- * RegisterFormula

fVariable :: Parser Formula
fVariable = FVar . T.pack <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

fInteger :: Parser Formula
fInteger = FConstant <$> lexeme L.decimal

fExpr :: Parser Formula
fExpr = makeExprParser fTerm fOperatorTable
  where fTerm :: Parser Formula
        fTerm = choice [ parens fExpr
                       , fVariable
                       , fInteger
                       ]

fOperatorTable :: [[Operator Parser Formula]]
fOperatorTable =
  [ [ binary "" FApply ]
  , [ prefix "-" FNeg
    , prefix "+" id
    ]
  , [ binary "*" FMul
    , binary "/" FDiv
    ]
  , [ binary "+" FAdd
    , binary "-" FSub
    ]
  , [ binary "," FPair ]
  ]
  where binary :: Text -> (Formula -> Formula -> Formula) -> Operator Parser Formula
        binary  name f = InfixL  (f <$ symbol name)
        prefix, postfix :: Text -> (Formula -> Formula) -> Operator Parser Formula
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)

parseF "" = pure $ FConstant 0 -- these can happen in registers
parseF s = parseMaybe (fExpr <* eof) s

debugParser = parseTest (fExpr <* eof) "3+woof(woof+2,2)"
