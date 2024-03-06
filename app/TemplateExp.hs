{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TemplateExp (
    expGen,
    eeeeeeee
)
where
import Language.Haskell.TH
    ( mkName,
      Q,
      Con(NormalC),
      Type(ConT, AppT),
      Dec(DataD, InstanceD, ClassD, SigD),
      Name,
      nameBase,
      Bang(Bang),
      SourceStrictness(NoSourceStrictness),
      SourceUnpackedness(NoSourceUnpackedness), DerivClause (DerivClause), Exp (ConE), Pat (VarP) )
--import Control.Monad.Cont (MonadTrans(lift))
import Control.Monad

getConstructor :: Name -> Con
getConstructor name = do
    let typename = nameBase name
    let constructorName = "From" ++ typename
    NormalC (mkName constructorName) [(Bang NoSourceUnpackedness NoSourceStrictness,ConT name)] 
expGen :: [Name] -> Q [Dec]
expGen names = do
    let constructors = map getConstructor names
    let name = mkName "Exp" 
    let qexpType :: Q Type = return $ ConT name
    let dataDeclaration = DataD [] name [] Nothing constructors [DerivClause Nothing [ConT ''Show]]
    classDeclaration <- [d|class Term a where
                                        toExp :: a -> $qexpType |]
    let ClassD _ className _ _ [SigD toExpName _] = head classDeclaration
    let expDeclaration name1 = ConE (mkName ("From" ++ nameBase name1))
    let funcDeclaration name1 = [d|$(return $ VarP toExpName) = $(return $ expDeclaration name1)|]
    let instanceDeclaration name1 = 
            do 
                decs <- funcDeclaration name1
                return $ InstanceD Nothing [] (AppT (ConT className) (ConT name1)) decs
    let concatM = liftM2 (++)
    foldl concatM (return []) [return [dataDeclaration], return classDeclaration, mapM instanceDeclaration names]
eeeeeeee :: Int -> Int
eeeeeeee x = x+1

    
    
