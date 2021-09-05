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
import Data.Functor.Identity

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

quote :: Parser a -> Parser a
quote = between (symbol "\"") (symbol "\"")

binary :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text Identity) a
binary  name f = InfixL  (f <$ symbol name)

prefix :: Text -> (a -> a) -> Operator (ParsecT Void Text Identity) a
prefix  name f = Prefix  (f <$ symbol name)

postfix :: Text -> (a -> a) -> Operator (ParsecT Void Text Identity) a
postfix name f = Postfix (f <$ symbol name)

-- * Constraints

cVariable :: Parser ResourceConstraint
cVariable = try (string "this" >> pure RCCollisionThis)
         <|> try (string "that" >> pure RCCollisionThat)
         <|> (RCVar . T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable"))

cTag :: Parser ResourceConstraint
cTag = RCTag . T.pack <$> quote (many alphaNumChar) 

cExpr :: Parser ResourceConstraint
cExpr = makeExprParser cTerm cOperatorTable
  where cTerm :: Parser ResourceConstraint
        cTerm = choice [ parens cExpr
                       , cTag
                       , cVariable
                       ]

cOperatorTable :: [[Operator Parser ResourceConstraint]]
cOperatorTable =
  [ [ binary "" RCApply ]
  , [ binary "==" RCEq ]
  , [ binary "&&" RCAnd ]
  ]

parseC :: Text -> Maybe ResourceConstraint
parseC = parseMaybe (cExpr <* eof)

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
  , [ prefix "==" (RFCondition CEqual)
    , prefix "!=" (RFCondition CNotEqual)
    , prefix ">=" (RFCondition CGtEq)
    , prefix "<=" (RFCondition CLtEq)
    , prefix ">"  (RFCondition CGt)
    , prefix "<"  (RFCondition CLt)
    ]
  , [ binary "+" RFAdd
    , binary "-" RFSubtract
    ]
  ]

-- | Parses plain RFs as they are in machinations
parseRFWithoutConstraints "" = pure $ RFConstant 0 -- Machinations allows empty edges :(
parseRFWithoutConstraints "all" = Just RFAll
parseRFWithoutConstraints s = parseMaybe (rExpr <* eof) s

-- | Parses RFs with added constraints
parseRF :: Text -> (Maybe ResourceFormula, Maybe ResourceConstraint)
parseRF t = case T.splitOn ";" t of
              [f] -> (parseRFWithoutConstraints f, Nothing)
              [f,c] -> (parseRFWithoutConstraints f, parseC c)

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
  , [ prefix ">=" (SFCondition CGtEq)
    , prefix "<=" (SFCondition CLtEq)
    , prefix "==" (SFCondition CEqual)
    , prefix "!=" (SFCondition CNotEqual)
    , prefix ">"  (SFCondition CGt)
    , prefix "<"  (SFCondition CLt)
    ]
  , [ binary ".." SFRange
    ]
  ]

parseSF "*" = Just SFTrigger
parseSF "!" = Just SFReverseTrigger
parseSF s = parseMaybe (sExpr <* eof) s

-- * RegisterFormula

fVariable :: Parser Formula
fVariable = FVar . T.pack <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

fInteger :: Parser Formula
fInteger = FConstant <$> (try (lexeme L.float) <|> lexeme L.decimal)

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

parseF "" = pure $ FConstant 0 -- these can happen in registers
parseF s = parseMaybe (fExpr <* eof) s

debugParser = parseTest (rExpr <* eof) "all;this(type)==\"bullet\""
