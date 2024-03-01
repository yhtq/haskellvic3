
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module BasePrinter where
import Data.Text ( Text )
import Data.Map (foldrWithKey, toAscList)
import Prettyprinter ( line, nest, Doc, Pretty(pretty) , (<+>), indent, lbrace, rbrace, vsep, dquotes, equals)
import BaseParser

type DocType = Doc Text

opMapInverse :: ValueOp -> Text
opMapInverse Add = "add"
opMapInverse Multiply = "multiply"
opMapInverse Subtract = "subtract"
opMapInverse Divide = "divide"
opMapInverse Min = "min"
opMapInverse Max = "max"
opMapInverse Modulo = "modulo"
opMapInverse Round = "round"
opMapInverse Ceiling = "ceiling"
opMapInverse Floor = "floor"
opMapInverse RoundTo = "round_to"

boolOpToText :: BoolOp -> Text
boolOpToText And = "AND"
boolOpToText Or = "OR"
boolOpToText Nor = "NOR"
boolOpToText NAND = "NAND"

cmpOpToText :: CmpOp -> Text
cmpOpToText Less = "<"
cmpOpToText LessEq = "<="
cmpOpToText Greater = ">"
cmpOpToText GreaterEq = ">="
cmpOpToText Eq = "="
cmpOpToText NotEq = "!="

testPrinter :: [Text] -> DocType
testPrinter (x: xs) = pretty x <> line <> (indent 4 $ testPrinter xs)
testPrinter [] = mempty

braced :: DocType -> DocType
braced d = lbrace <> line <> (indent 4 d) <> line <> rbrace


printObject :: Object -> DocType
printObject (Object "" attrs) = (braced $ printAttrs attrs)
printObject (Object name attrs) = pretty name <+> equals <+> (braced $ printAttrs attrs)
printObjectInList :: ObjectInList -> DocType
printObjectInList (ObjectInList "" attrs) = (braced $ printAttrsList attrs)
printObjectInList (ObjectInList name attrs) = pretty name <+> equals <+> (braced $ printAttrsList attrs)

printAttrs :: DefinitionMap -> DocType
printAttrs = printAttrsList.toAscList 
printAttrsList :: [Dec] -> DocType
printAttrsList l = vsep $ map (\(k, v) -> pretty k <+> equals <+> printExp v) l
-- 这些 undefined 是不应发生的意思，后续尝试给出更多信息
__printExp :: Exp -> DocType
__printExp (FromValueUntypedNumExp e) = undefined
__printExp (FromValueIntExp e) = printValueIntExp e
__printExp (FromValueFloatExp e) = printValueFloatExp e
__printExp (FromConstBoolExp e) = printConstBoolExp e
__printExp (FromPossibleExp e) = undefined
__printExp (FromSwitch e) = undefined
__printExp (FromObject e) = printObject e
__printExp (FromObjectInList e) = printObjectInList e
__printExp (FromVar e) = printVar e
__printExp (FromColor e) = printColor e
__printExp (FromIdentifier e) = pretty e
__printExp (FromText e) = dquotes $ pretty e
__printExp (FromGroups e) = printGroups e
printExp :: Exp -> DocType
printExp = __printExp

-- 这里统一不添加大括号，只做换行处理
printValueExpWithoutBrace :: (ValueNum a, Pretty a) => ValueExp a -> DocType
printValueExpWithoutBrace (ValueExpWithDesc desc e) = printValueExpWithoutBrace e <> line <> pretty ("desc =" :: Text) <+> pretty desc
printValueExpWithoutBrace (RawStaticalValue v) = pretty $ printNum v
printValueExpWithoutBrace (RawIdentifier v) = pretty v
printValueExpWithoutBrace (RawScriptedValue v) = pretty v
printValueExpWithoutBrace (Exp e appending) = "value = " <> printValueExp e <> line <> printAppending appending
printValueExpWithoutBrace (IfExp cond if_stru) = undefined
printValueExpWithoutBrace (AppendIfExp _ _ ) = undefined

-- 这是处理大括号的版本，只在需要时添加
printValueExp :: (ValueNum a, Pretty a) => ValueExp a -> DocType
printValueExp e@(ValueExpWithDesc _ _) = braced $ printValueExpWithoutBrace e
printValueExp e@(Exp _ _) = braced $ printValueExpWithoutBrace e
printValueExp e = printValueExpWithoutBrace e

printAppending :: (ValueNum a, Pretty a) => AppendingValueExp a -> DocType
printAppending (ExpAppending op e) = (pretty $ opMapInverse op) <+> equals <+> printValueExp e
printAppending (ExpAppendings app op e) = printAppending app <> line <> printValueExp e

printValueIntExp :: ValueIntExp -> DocType
printValueIntExp = printValueExp
printValueFloatExp :: ValueFloatExp -> DocType
printValueFloatExp = printValueExp

printConstBoolExp :: ConstBoolExp -> DocType
printConstBoolExp (BoolRaw Yes) = pretty ("yes" :: Text)
printConstBoolExp (BoolRaw No) = pretty ("no" :: Text)

printVar :: Var -> DocType
printVar (Var name "") = pretty name
printVar (Var name t) = pretty t <> ":" <> pretty name

printColor :: Color -> DocType
printColor _ = undefined

printGroups :: Groups -> DocType
printGroups = vsep.(map pretty)

printBoolExp :: BoolExp -> DocType
printBoolExp (AndList l) = vsep $ map printBoolExp l
printBoolExp (BoolOp op l) = pretty (boolOpToText op) <+> equals <+> braced (vsep $ map printBoolExp l)
printBoolExp (BoolOp' not e) = "NOT" <+> equals <+> braced (printBoolExp e)
printBoolExp (IntCmp op l r) = undefined
printBoolExp (FloatCmp op l r) = undefined
printBoolExp (Q q v) = printVar q <+> equals <+> printVar v
printBoolExp (ScopeTrans (ScopeTransformer scope name) e) = 
    prefix <+> pretty name <+> equals <+> braced (printBoolExp e) where 
        prefix = if scope == "" then mempty else pretty scope <> ":"

class ParadoxPrintable a where
    printParadox :: a -> DocType
instance {-# OVERLAPPING #-} ParadoxPrintable ObjectInList where
    printParadox = printObjectInList
instance ParadoxPrintable Exp where
    printParadox = printExp
instance (ValueNum a, Pretty a) => ParadoxPrintable (ValueExp a) where
    printParadox = printValueExp
instance ParadoxPrintable Var where
    printParadox = printVar
instance ParadoxPrintable Color where
    printParadox = printColor
instance ParadoxPrintable Groups where
    printParadox = printGroups
instance ParadoxPrintable BoolExp where
    printParadox = printBoolExp
instance {-# INCOHERENT #-} (ParadoxPrintable a) => ParadoxPrintable [a] where
    printParadox = vsep.(map printParadox)
