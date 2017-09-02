{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Language.WebIDL.Grammar where -- (Tag(..), Comment(..), WebIDL(..), grammar, parseIDL) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2, optional)
import Control.Monad (guard)
import Data.Char (isAlphaNum, isDigit, isHexDigit, isLetter)
import Data.Function (fix)
import Data.Functor.Compose (getCompose)
import Data.Monoid((<>))
import Data.Set (Set, fromList, notMember)
import qualified Rank2.TH
import Text.Grampa
--import Text.Grampa.ContextFree.LeftRecursive (Parser)
import Text.Grampa.ContextFree.Memoizing (Parser)
--import Text.Grampa.ContextFree.Parallel (Parser)
--import Text.Grampa.PEG.Packrat (Parser)
--import Text.Grampa.PEG.Backtrack (Parser)
import Text.Parsec.Pos(initialPos)
import Text.Parser.Char (char, oneOf)
import Text.Parser.Combinators (choice, count, manyTill, sepBy, skipMany, try)
import Language.WebIDL.AST
import Language.WebIDL.Parser (Tag(..))

import Prelude hiding (Enum, exponent)

spaces = whiteSpace

data Comment = LineComment String | BlockComment String deriving Show

data WebIDL p = WebIDL {
  pIDL :: p [Definition Tag],
  pDef :: p (Definition Tag),
  pCallback :: p (Callback Tag),
  pExtAttrs :: p [ExtendedAttribute Tag],
  pExtAttr :: p (ExtendedAttribute Tag),
  pPartial :: p (Partial Tag),
  pDictionary :: p (Dictionary Tag),
  pInterface :: p (Interface Tag),
  pException :: p (Exception Tag),
  pInheritance :: p (Maybe Ident),
  pEnum :: p (Enum Tag),
  pEnumValues :: p [EnumValue],
  pTypedef :: p (Typedef Tag),
  pImplementsStatement :: p (ImplementsStatement Tag),
  pDictionaryMember :: p (DictionaryMember Tag),
  pExceptionMember :: p (ExceptionMember Tag),
  pMaybeIdent :: p (Maybe Ident),
  pInterfaceMember :: p (InterfaceMember Tag),
  pConst :: p (Const Tag),
  pConstType :: p ConstType,
  pAttribute :: p (Attribute Tag),
  pOperation :: p (Operation Tag),
  pArg :: p (Argument Tag),
  pArgumentName :: p ArgumentName,
  pArgumentNameKeyword :: p ArgumentNameKeyword,
  pDefault :: p (Maybe Default),
  pQualifier :: p (Maybe Qualifier),
  pSpecial :: p Special,
  pReturnType :: p ReturnType,
  pConstValue :: p ConstValue,
  pBool :: p Bool,
  pNull :: p (Maybe Null),
  pPrimTy :: p PrimitiveType,
  pIntegerType :: p IntegerType,
  pUnsigned :: p (Maybe Unsigned),
  pFloatType :: p FloatType,
  pType :: p Type,
  pSingleType :: p SingleType,
  pNonAnyType :: p NonAnyType,
  pTypeSuffix :: p TypeSuffix,
  -- FIXME: Not working correctly currently,
  pUnionType :: p UnionType,
  pUnionMemberType :: p UnionMemberType
  }

$(Rank2.TH.deriveAll ''WebIDL)

parseIDL :: String -> Either ParseFailure [[Definition Tag]]
parseIDL = getCompose . pIDL . parseComplete (fixGrammar grammar)
--parseIDL = fmap (:[]) . pIDL . parseComplete (fix grammar)

pModifier m s = optional (string s *> pSpaces *> return m)
pParenComma p = parens (pSpaces *> sepBy (p <* pSpaces) (char ',' <* pSpaces))

moptional :: (Monoid x, Alternative p) => p x -> p x
moptional p = p <|> pure mempty

sepBy1 :: Alternative p => p x -> p sep -> p [x]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

grammar :: -- (Eq t, Show t, TextualMonoid t) =>
           GrammarBuilder WebIDL WebIDL Parser String
grammar ~WebIDL{..} = WebIDL{
  pIDL = pSpaces *> some (pDef <* pSpaces),
  pDef = try (DefInterface <$> pInterface)
     <|> DefCallback <$> pCallback
     <|> DefPartial <$> pPartial
     <|> DefDictionary <$> pDictionary
     <|> DefException <$> pException
     <|> DefEnum <$> pEnum
     <|> DefTypedef <$> pTypedef
     <|> DefImplementsStatement <$> pImplementsStatement,
  pCallback = Callback <$> (string "callback" *> pSpaces *> getTag)
                       <*> pIdent
                       <*> (pEq *> pReturnType <* pSpaces)
                       <*> pParenComma pArg <* semi,
  pExtAttrs = try (brackets (pSpaces *> sepBy (pExtAttr <* pSpaces) (char ',' <* pSpaces)))
          <|> return [],
  pExtAttr = try (ExtendedAttributeNamedArgList <$> getTag <*> (pIdent <* pEq) <*> pIdent <*> pParenComma pArg)
         <|> try (ExtendedAttributeArgList <$> getTag <*> pIdent <*> pParenComma pArg)
         <|> try (ExtendedAttributeIdent <$> getTag <*> (pIdent <* pEq) <*> pIdent)
         <|> try (ExtendedAttributeIdentList <$> getTag <*> (pIdent <* pEq) <*> pParenComma pIdent)
         <|> ExtendedAttributeNoArgs <$> getTag <*> pIdent,
  pPartial = pExtAttrs *> string "partial" *> pSpaces *>
             (PartialInterface <$> getTag <*> (string "interface" *> pSpaces *> pIdent)
                                <*> braces (many pInterfaceMember) <* semi
              <|> PartialDictionary <$> getTag <*> (string "dictionary" *> pSpaces *> pIdent)
                                 <*> braces (many pDictionaryMember) <* semi),
  pDictionary = Dictionary <$> getTag <*> (string "dictionary" *> pSpaces *> pIdent)
                           <*> pInheritance <*> braces (many pDictionaryMember) <* semi,
  pInterface = Interface <$> getTag <*> pExtAttrs <*> (string "interface" *> pSpaces *> pIdent)
                         <*> pInheritance <*> braces (many (pInterfaceMember <* pSpaces)) <* semi,
  pException = Exception <$> getTag <*> (string "exception" *> pSpaces *> pIdent)
                            <*> pInheritance <*> braces (many pExceptionMember),
  pInheritance = optional (spaces *> char ':'  *> spaces *> pIdent),
  pEnum = Enum <$> getTag <*> (string "enum" *> pSpaces *> pIdent) <*> braces pEnumValues <* semi,
  pEnumValues = sepBy1 (EnumValue <$> stringLit) (char ','),
  pTypedef = do
    tag <- getTag
    _ <- string "typedef"
    pSpaces
    ty <- try pType
    pSpaces
    ident <- pIdent
    _ <- semi
    return (Typedef tag ty ident),
  pImplementsStatement = ImplementsStatement <$> getTag <*> pIdent <* pSpaces
                                                <*> (string "implements" *> pSpaces *> pIdent <* semi),
  pDictionaryMember = DictionaryMember <$> getTag <*> pType <* pSpaces
                                       <*> pIdent <*> pDefault <* semi,
  pExceptionMember =  ExConst <$> getTag <*> pConst
                  <|> ExField <$> getTag <*> pType <*> pIdent <* semi,
  pMaybeIdent = optional pIdent,
  pInterfaceMember =  try (IMemConst <$> pConst)
                  <|> try (IMemAttribute <$> pAttribute)
                  <|> IMemOperation <$> pOperation,
  pConst = Const <$> getTag <*> (string "const" *> pSpaces *> pConstType <* pSpaces)
                 <*> (pIdent <* pEq) <*> (pSpaces *> pConstValue <* semi),
  pConstType =  ConstPrim <$> pPrimTy <*> pNull
            <|> ConstIdent <$> pIdent <*> pNull,
  pAttribute = Attribute <$> getTag <*> pModifier Inherit "inherit"
                         <*> pModifier ReadOnly "readonly"
                         <*> (string "attribute" *> pSpaces *> pType)
                         <*> (pSpaces *> pIdent <* semi),
  pOperation = Operation <$> getTag <*> pExtAttrs <*> pQualifier <* spaces
                         <*> pReturnType <* pSpaces
                         <*> pMaybeIdent <* pSpaces
                         <*> pParenComma pArg <* semi,
  pArg =  try (ArgOptional <$> pExtAttrs <*> (string "optional" *> spaces *> pType <* pSpaces) <*> pArgumentName <*> pDefault)
      <|> ArgNonOpt <$> pExtAttrs <*> (pType <* pSpaces) <*> (pModifier Ellipsis "...") <*> pArgumentName,
  pArgumentName = try (ArgKey <$> pArgumentNameKeyword)
              <|> ArgIdent <$> pIdent,
  pArgumentNameKeyword =  string "attribute" *> return ArgAttribute
                      <|> string "callback" *> return ArgCallback
                      <|> string "const" *> return ArgConst
                      <|> string "creator" *> return ArgCreator
                      <|> string "deleter" *> return ArgDeleter
                      <|> string "dictionary" *> return ArgDictionary
                      <|> string "enum" *> return ArgEnum
                      <|> string "exception" *> return ArgException  
                      <|> string "getter" *> return ArgGetter
                      <|> string "implements" *> return ArgImplements
                      <|> string "inherit" *> return ArgInherit
                      <|> string "interface" *> return ArgInterface  
                      <|> string "legacycaller" *> return ArgLegacycaller
                      <|> string "partial" *> return ArgPartial
                      <|> string "setter" *> return ArgSetter
                      <|> string "static" *> return ArgStatic 
                      <|> string "stringifier" *> return ArgStringifier
                      <|> string "typedef" *> return ArgTypedef
                      <|> string "unrestricted" *> return ArgUnrestricted,
  pDefault = Just <$> (spaces *> pEq *> spaces *> (DefaultValue <$> pConstValue
                                                   <|> DefaultString <$> stringLit))
             <|> return Nothing,
  pQualifier =  try (string "static" *> return (Just QuaStatic))
            <|> try (Just . QSpecials <$> some pSpecial)
            <|> return Nothing,
  pSpecial = string "getter" *> return Getter
         <|> string "setter" *> return Setter
         <|> string "ccreator" *> return Ccreator
         <|> string "deleter" *> return Deleter
         <|> string "legacycaller" *> return Legacycaller,
  pReturnType = string "void" *> return RetVoid
            <|> RetType <$> pType,
  pConstValue =  ConstBooleanLiteral <$> pBool
             <|> try (ConstFloatLiteral <$> pFloat)
             <|> ConstInteger <$> pInt
             <|> string "null" *> return ConstNull,
  pBool =  string "true" *> return True
       <|> string "false" *> return False,
  pNull = optional (char '?' *> return Null),
  pPrimTy = try (string "boolean" *> return Boolean)
        <|> try (string "byte" *> return Byte)
        <|> try (string "octet" *> return Octet)
        <|> try (PrimIntegerType <$> pIntegerType)
        <|> PrimFloatType <$> pFloatType,
  pIntegerType = IntegerType <$> pUnsigned <* pSpaces 
                 <*> (string "short" *> return Short
                      <|> Long . length <$> some (try (string "long" <* pSpaces))),
  pUnsigned = optional (string "unsigned" *> return Unsigned),
  pFloatType =  try (TyFloat <$> pModifier Unrestricted "unrestricted" <* spaces <* string "float")
            <|> TyDouble <$> pModifier Unrestricted "unrestricted" <* spaces <* string "double",
  pType =  TySingleType <$> pSingleType
       <|> TyUnionType <$> pUnionType <*> pTypeSuffix,
  pSingleType =  STyAny <$> (string "any" *> pTypeSuffix)
             <|> STyNonAny <$> pNonAnyType,
  pNonAnyType =  try (TyPrim <$> pPrimTy <*> pTypeSuffix)
             <|> TySequence <$> (string "sequence" *> pSpaces *> string "<" *> spaces *> pType <* string ">" <* spaces) <*> pNull
             <|> TyObject <$> (string "object" *> pTypeSuffix)
             <|> try (TyDOMString <$> (string "DOMString" *> pTypeSuffix))
             <|> try (TyDate <$> (string "Date" *> pTypeSuffix))
             <|> TyIdent <$> pIdent <*> pTypeSuffix,
  pTypeSuffix =  try (string "[]" *> return TypeSuffixArray)
             <|> try (char '?' *> return TypeSuffixNullable)
             <|> return TypeSuffixNone,
  -- FIXME: Not working correctly currently,
  pUnionType = parens (sepBy1 pUnionMemberType (spaces *> string "or" <* spaces)),
  pUnionMemberType =  UnionTy <$> pUnionType <*> pTypeSuffix
                  <|> UnionTyNonAny <$> pNonAnyType
                  <|> UnionTyAny <$> (string "any []" *> pTypeSuffix)
  {-
  pLineComment = do
    _ <- string "//"
    comment <- manyTill anyChar (try newline)
    return ()
    modifyState (\ps -> ParserState { _comments' = _comments' ps ++ [LineComment comment]}),
  pBlockComment = do
    _ <- string "/*"
    comment <- manyTill anyChar (try (string "*/"))
    modifyState (\ps -> ParserState { _comments' = _comments' ps ++ [BlockComment comment]}),
  getTag = do
    pos <- getPosition
    ParserState comments <- getState
    putState $ ParserState []
    return $ Tag comments pos,
  pString = many anyChar
  -}
}

pInt      :: Parser WebIDL String Integer
pFloat    :: Parser WebIDL String Double
pIdent     = do name <- (moptional (string "_") <> satisfyCharInput isLetter
                         <> takeCharsWhile (\c-> isLetter c || isDigit c || c == '-' ||  c == '_'))
                guard (notMember name reservedKeywordSet)
                spaces
                pure (Ident name)
pInt       = read <$> (moptional (string "-")
                       <> (satisfyCharInput (liftA2 (&&) (>'0') (<='9')) <> takeCharsWhile isDigit <|>
                           string "0" <> (string "x" <|> string "X") <> takeCharsWhile1 isHexDigit <|>
                           string "0" <> takeCharsWhile (liftA2 (&&) (>'0') (<='7'))))
             <* spaces
pFloat     = read <$>
             let exponent = (string "e" <|> string "E") 
                            <> moptional (string "+" <|> string "-") <> takeCharsWhile1 isDigit
             in moptional (string "-") 
                <> (takeCharsWhile1 isDigit <> string "." <> takeCharsWhile isDigit <|>
                    takeCharsWhile isDigit <> string "." <> takeCharsWhile1 isDigit)
                <> moptional exponent
                <|> takeCharsWhile1 isDigit <> exponent
             <* spaces
semi       = string ";" <* spaces
stringLit  = string "\"" *> takeCharsWhile (/= '"') <* string "\""
             <* spaces
pEq        = spaces *> char '=' <* spaces

getTag = pure (Tag [] $ initialPos "input")
parens p = string "(" *> spaces *> p <* string ")" <* spaces
brackets p = string "[" *> spaces *> p <* string "]" <* spaces
braces p = string "{" *> pSpaces *> p <* string "}" <* spaces

pSpaces = try (spaces *> skipMany (pComment <* spaces))
pComment = try pLineComment <|> pBlockComment
pLineComment = string "//" *> takeCharsWhile (/= '\n') *> pure ()
pBlockComment = string "/*" *> skipMany (notFollowedBy (string "*/") *> takeCharsWhile (/= '*')) <* string "*/"

reservedKeywordSet = fromList ["attribute" , "-" , "-Infinity" , "." , "..." , ":" , ";" , "<" , "=" , ">" , "?" , "ByteString" , "DOMString" , "Infinity" , "NaN", "any" , "boolean" , "byte" , "double" , "false" , "float" , "long" , "null" , "object" , "octet" , "or" , "optional" , "sequence" , "short" , "true" , "unsigned" , "void", "attribute" , "callback" , "const" , "deleter" , "dictionary" , "enum" , "getter" , "implements" , "inherit" , "interface" , "iterable" , "legacycaller" , "partial" , "required" , "serializer" , "setter" , "static" , "stringifier" , "typedef" , "unrestricted"]

reservedKeywords, argumentNameKeyword, bufferRelatedType :: Parser WebIDL String String
reservedKeywords = string "attribute"
 <|> string "-"
 <|> string "-Infinity"
 <|> string "."
 <|> string "..."
 <|> string ":"
 <|> string ";"
 <|> string "<"
 <|> string "="
 <|> string ">"
 <|> string "?"
 <|> string "ByteString"
 <|> string "DOMString"
 <|> string "Infinity"
 <|> string "NaN"
-- <|> string "USVString"
 <|> string "any"
 <|> string "boolean"
 <|> string "byte"
 <|> string "double"
 <|> string "false"
 <|> string "float"
 <|> string "long"
 <|> string "null"
 <|> string "object"
 <|> string "octet"
 <|> string "or"
 <|> string "optional"
 <|> string "sequence"
 <|> string "short"
 <|> string "true"
 <|> string "unsigned"
 <|> string "void"
 <|> argumentNameKeyword
-- <|> bufferRelatedType

argumentNameKeyword = string "attribute"
 <|> string "callback"
 <|> string "const"
 <|> string "deleter"
 <|> string "dictionary"
 <|> string "enum"
 <|> string "getter"
 <|> string "implements"
 <|> string "inherit"
 <|> string "interface"
 <|> string "iterable"
 <|> string "legacycaller"
 <|> string "partial"
 <|> string "required"
 <|> string "serializer"
 <|> string "setter"
 <|> string "static"
 <|> string "stringifier"
 <|> string "typedef"
 <|> string "unrestricted"

bufferRelatedType = string "ArrayBuffer"
 <|> string "DataView"
 <|> string "Int8Array"
 <|> string "Int16Array"
 <|> string "Int32Array"
 <|> string "Uint8Array"
 <|> string "Uint16Array"
 <|> string "Uint32Array"
 <|> string "Uint8ClampedArray"
 <|> string "Float32Array"
 <|> string "Float64Array"

{-
[1] 	Definitions 	→ 	ExtendedAttributeList Definition Definitions
 | ε
[2] 	Definition 	→ 	CallbackOrInterface
 | Partial
 | Dictionary
 | Enum
 | Typedef
 | ImplementsStatement
[3] 	CallbackOrInterface 	→ 	"callback" CallbackRestOrInterface
 | Interface
[4] 	CallbackRestOrInterface 	→ 	CallbackRest
 | Interface
[5] 	Interface 	→ 	"interface" identifier Inheritance "{" InterfaceMembers "}" ";"
[6] 	Partial 	→ 	"partial" PartialDefinition
[7] 	PartialDefinition 	→ 	PartialInterface
 | PartialDictionary
[8] 	PartialInterface 	→ 	"interface" identifier "{" InterfaceMembers "}" ";"
[9] 	InterfaceMembers 	→ 	ExtendedAttributeList InterfaceMember InterfaceMembers
 | ε
[10] 	InterfaceMember 	→ 	Const
 | Operation
 | Serializer
 | Stringifier
 | StaticMember
 | Iterable
 | ReadOnlyMember
 | ReadWriteAttribute
[11] 	Dictionary 	→ 	"dictionary" identifier Inheritance "{" DictionaryMembers "}" ";"
[12] 	DictionaryMembers 	→ 	ExtendedAttributeList DictionaryMember DictionaryMembers
 | ε
[13] 	DictionaryMember 	→ 	Required Type identifier Default ";"
[14] 	Required 	→ 	"required"
 | ε
[15] 	PartialDictionary 	→ 	"dictionary" identifier "{" DictionaryMembers "}" ";"
[16] 	Default 	→ 	"=" DefaultValue
 | ε
[17] 	DefaultValue 	→ 	ConstValue
 | string
 | "[" "]"
[18] 	Inheritance 	→ 	":" identifier
 | ε
[19] 	Enum 	→ 	"enum" identifier "{" EnumValueList "}" ";"
[20] 	EnumValueList 	→ 	string EnumValueListComma
[21] 	EnumValueListComma 	→ 	"," EnumValueListString
 | ε
[22] 	EnumValueListString 	→ 	string EnumValueListComma
 | ε
[23] 	CallbackRest 	→ 	identifier "=" ReturnType "(" ArgumentList ")" ";"
[24] 	Typedef 	→ 	"typedef" Type identifier ";"
[25] 	ImplementsStatement 	→ 	identifier "implements" identifier ";"
[26] 	Const 	→ 	"const" ConstType identifier "=" ConstValue ";"
[27] 	ConstValue 	→ 	BooleanLiteral
 | FloatLiteral
 | integer
 | "null"
[28] 	BooleanLiteral 	→ 	"true"
 | "false"
[29] 	FloatLiteral 	→ 	float
 | "-Infinity"
 | "Infinity"
 | "NaN"
[30] 	Serializer 	→ 	"serializer" SerializerRest
[31] 	SerializerRest 	→ 	OperationRest
 | "=" SerializationPattern ";"
 | ";"
[32] 	SerializationPattern 	→ 	"{" SerializationPatternMap "}"
 | "[" SerializationPatternList "]"
 | identifier
[33] 	SerializationPatternMap 	→ 	"getter"
 | "inherit" Identifiers
 | identifier Identifiers
 | ε
[34] 	SerializationPatternList 	→ 	"getter"
 | identifier Identifiers
 | ε
[35] 	Stringifier 	→ 	"stringifier" StringifierRest
[36] 	StringifierRest 	→ 	ReadOnly AttributeRest
 | ReturnType OperationRest
 | ";"
[37] 	StaticMember 	→ 	"static" StaticMemberRest
[38] 	StaticMemberRest 	→ 	ReadOnly AttributeRest
 | ReturnType OperationRest
[39] 	ReadOnlyMember 	→ 	"readonly" ReadOnlyMemberRest
[40] 	ReadOnlyMemberRest 	→ 	AttributeRest
[41] 	ReadWriteAttribute 	→ 	"inherit" ReadOnly AttributeRest
 | AttributeRest
[42] 	AttributeRest 	→ 	"attribute" Type AttributeName ";"
[43] 	AttributeName 	→ 	AttributeNameKeyword
 | identifier
[44] 	AttributeNameKeyword 	→ 	"required"
[45] 	Inherit 	→ 	"inherit"
 | ε
[46] 	ReadOnly 	→ 	"readonly"
 | ε
[47] 	Operation 	→ 	ReturnType OperationRest
 | SpecialOperation
[48] 	SpecialOperation 	→ 	Special Specials ReturnType OperationRest
[49] 	Specials 	→ 	Special Specials
 | ε
[50] 	Special 	→ 	"getter"
 | "setter"
 | "deleter"
 | "legacycaller"
[51] 	OperationRest 	→ 	OptionalIdentifier "(" ArgumentList ")" ";"
[52] 	OptionalIdentifier 	→ 	identifier
 | ε
[53] 	ArgumentList 	→ 	Argument Arguments
 | ε
[54] 	Arguments 	→ 	"," Argument Arguments
 | ε
[55] 	Argument 	→ 	ExtendedAttributeList OptionalOrRequiredArgument
[56] 	OptionalOrRequiredArgument 	→ 	"optional" Type ArgumentName Default
 | Type Ellipsis ArgumentName
[57] 	ArgumentName 	→ 	ArgumentNameKeyword
 | identifier
[58] 	Ellipsis 	→ 	"..."
 | ε
[59] 	Iterable 	→ 	"iterable" "<" Type OptionalType ">" ";"
[60] 	OptionalType 	→ 	"," Type
 | ε
[65] 	ExtendedAttributeList 	→ 	"[" ExtendedAttribute ExtendedAttributes "]"
 | ε
[66] 	ExtendedAttributes 	→ 	"," ExtendedAttribute ExtendedAttributes
 | ε
[67] 	ExtendedAttribute 	→ 	"(" ExtendedAttributeInner ")" ExtendedAttributeRest
 | "[" ExtendedAttributeInner "]" ExtendedAttributeRest
 | "{" ExtendedAttributeInner "}" ExtendedAttributeRest
 | Other ExtendedAttributeRest
[68] 	ExtendedAttributeRest 	→ 	ExtendedAttribute
 | ε
[69] 	ExtendedAttributeInner 	→ 	"(" ExtendedAttributeInner ")" ExtendedAttributeInner
 | "[" ExtendedAttributeInner "]" ExtendedAttributeInner
 | "{" ExtendedAttributeInner "}" ExtendedAttributeInner
 | OtherOrComma ExtendedAttributeInner
 | ε
[70] 	Other 	→ 	integer
 | float
 | identifier
 | string
 | other
 | "-"
 | "-Infinity"
 | "."
 | "..."
 | ":"
 | ";"
 | "<"
 | "="
 | ">"
 | "?"
 | "ByteString"
 | "DOMString"
 | "Infinity"
 | "NaN"
 | "USVString"
 | "any"
 | "boolean"
 | "byte"
 | "double"
 | "false"
 | "float"
 | "long"
 | "null"
 | "object"
 | "octet"
 | "or"
 | "optional"
 | "sequence"
 | "short"
 | "true"
 | "unsigned"
 | "void"
 | ArgumentNameKeyword
 | BufferRelatedType
[71] 	ArgumentNameKeyword 	→ 	"attribute"
 | "callback"
 | "const"
 | "deleter"
 | "dictionary"
 | "enum"
 | "getter"
 | "implements"
 | "inherit"
 | "interface"
 | "iterable"
 | "legacycaller"
 | "partial"
 | "required"
 | "serializer"
 | "setter"
 | "static"
 | "stringifier"
 | "typedef"
 | "unrestricted"
[72] 	OtherOrComma 	→ 	Other
 | ","
[73] 	Type 	→ 	SingleType
 | UnionType Null
[74] 	SingleType 	→ 	NonAnyType
 | "any"
[75] 	UnionType 	→ 	"(" UnionMemberType "or" UnionMemberType UnionMemberTypes ")"
[76] 	UnionMemberType 	→ 	NonAnyType
 | UnionType Null
[77] 	UnionMemberTypes 	→ 	"or" UnionMemberType UnionMemberTypes
 | ε
[78] 	NonAnyType 	→ 	PrimitiveType Null
 | PromiseType Null
 | "ByteString" Null
 | "DOMString" Null
 | "USVString" Null
 | identifier Null
 | "sequence" "<" Type ">" Null
 | "object" Null
 | "Error" Null
 | "DOMException" Null
 | BufferRelatedType Null
[79] 	BufferRelatedType 	→ 	"ArrayBuffer"
 | "DataView"
 | "Int8Array"
 | "Int16Array"
 | "Int32Array"
 | "Uint8Array"
 | "Uint16Array"
 | "Uint32Array"
 | "Uint8ClampedArray"
 | "Float32Array"
 | "Float64Array"
[80] 	ConstType 	→ 	PrimitiveType Null
 | identifier Null
[81] 	PrimitiveType 	→ 	UnsignedIntegerType
 | UnrestrictedFloatType
 | "boolean"
 | "byte"
 | "octet"
[82] 	UnrestrictedFloatType 	→ 	"unrestricted" FloatType
 | FloatType
[83] 	FloatType 	→ 	"float"
 | "double"
[84] 	UnsignedIntegerType 	→ 	"unsigned" IntegerType
 | IntegerType
[85] 	IntegerType 	→ 	"short"
 | "long" OptionalLong
[86] 	OptionalLong 	→ 	"long"
 | ε
[87] 	PromiseType 	→ 	"Promise" "<" ReturnType ">"
[88] 	Null 	→ 	"?"
 | ε
[89] 	ReturnType 	→ 	Type
 | "void"
[90] 	IdentifierList 	→ 	identifier Identifiers
[91] 	Identifiers 	→ 	"," identifier Identifiers
 | ε
[92] 	ExtendedAttributeNoArgs 	→ 	identifier
[93] 	ExtendedAttributeArgList 	→ 	identifier "(" ArgumentList ")"
[94] 	ExtendedAttributeIdent 	→ 	identifier "=" identifier
[95] 	ExtendedAttributeIdentList 	→ 	identifier "=" "(" IdentifierList ")"
[96] 	ExtendedAttributeNamedArgList 	→ 	identifier "=" identifier "(" ArgumentList ")" 
-}
