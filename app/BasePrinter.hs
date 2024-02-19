
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module BasePrinter where
import Data.Text ( Text )
import Data.Map (foldrWithKey, toAscList)
import Prettyprinter ( line, nest, Doc, Pretty(pretty) , (<+>), indent, lbrace, rbrace, vsep, dquotes, equals)
import BaseParser

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

testPrinter :: [Text] -> Doc Text
testPrinter (x: xs) = pretty x <> line <> (indent 4 $ testPrinter xs)
testPrinter [] = mempty

braced :: Doc Text -> Doc Text
braced d = lbrace <> line <> (indent 4 d) <> line <> rbrace


printObject :: Object -> Doc Text
printObject (Object "" attrs) = (braced $ printAttrs attrs)
printObject (Object name attrs) = pretty name <+> equals <+> (braced $ printAttrs attrs)
printObjectInList :: ObjectInList -> Doc Text
printObjectInList (ObjectInList "" attrs) = (braced $ printAttrsList attrs)
printObjectInList (ObjectInList name attrs) = pretty name <+> equals <+> (braced $ printAttrsList attrs)

printAttrs :: DefinitionMap -> Doc Text
printAttrs = printAttrsList.toAscList 
printAttrsList :: [(Key, Exp)] -> Doc Text
printAttrsList l = vsep $ map (\(k, v) -> pretty k <+> equals <+> printExp v) l
-- 这些 undefined 是不应发生的意思，后续尝试给出更多信息
__printExp :: Exp -> Doc Text
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
__printExp (FromText e) = dquotes $ pretty e
__printExp (FromGroups e) = printGroups e
printExp :: Exp -> Doc Text
printExp = __printExp

-- 这里统一不添加大括号，只做换行处理
printValueExpWithoutBrace :: Pretty a => ValueExp a -> Doc Text
printValueExpWithoutBrace (ValueExpWithDesc desc e) = printValueExpWithoutBrace e <> line <> pretty ("desc =" :: Text) <+> pretty desc
printValueExpWithoutBrace (RawStaticalValue v) = pretty v
printValueExpWithoutBrace (RawIdentifier v) = pretty v
printValueExpWithoutBrace (RawScriptedValue v) = pretty v
printValueExpWithoutBrace (Exp e appending) = "value = " <> printValueExp e <> line <> printAppending appending
printValueExpWithoutBrace (IfExp cond if_stru) = undefined
printValueExpWithoutBrace (AppendIfExp _ _ ) = undefined

-- 这是处理大括号的版本，只在需要时添加
printValueExp :: Pretty a => ValueExp a -> Doc Text
printValueExp e@(ValueExpWithDesc _ _) = braced $ printValueExpWithoutBrace e
printValueExp e@(Exp _ _) = braced $ printValueExpWithoutBrace e
printValueExp e = printValueExpWithoutBrace e

printAppending :: Pretty a => AppendingValueExp a -> Doc Text
printAppending (ExpAppending op e) = (pretty $ opMapInverse op) <+> equals <+> printValueExp e
printAppending (ExpAppendings app op e) = printAppending app <> line <> printValueExp e

printValueIntExp :: ValueIntExp -> Doc Text
printValueIntExp = printValueExp
printValueFloatExp :: ValueFloatExp -> Doc Text
printValueFloatExp = printValueExp

printConstBoolExp :: ConstBoolExp -> Doc Text
printConstBoolExp (BoolRaw Yes) = pretty ("yes" :: Text)
printConstBoolExp (BoolRaw No) = pretty ("no" :: Text)

printVar :: Var -> Doc Text
printVar (Var name "") = pretty name
printVar (Var name t) = pretty t <> ":" <> pretty name

printColor :: Color -> Doc Text
printColor _ = undefined

printGroups :: Groups -> Doc Text
printGroups = vsep.(map pretty)

printBoolExp :: BoolExp -> Doc Text
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
    printParadox :: a -> Doc Text
instance {-# OVERLAPPING #-} ParadoxPrintable ObjectInList where
    printParadox = printObjectInList
instance ParadoxPrintable Exp where
    printParadox = printExp
instance (Pretty a) => ParadoxPrintable (ValueExp a) where
    printParadox = printValueExp
instance ParadoxPrintable Var where
    printParadox = printVar
instance ParadoxPrintable Color where
    printParadox = printColor
instance ParadoxPrintable Groups where
    printParadox = printGroups
instance ParadoxPrintable BoolExp where
    printParadox = printBoolExp
instance (ParadoxPrintable a) => ParadoxPrintable [a] where
    printParadox = vsep.(map printParadox)