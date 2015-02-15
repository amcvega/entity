{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Entity.TH where

import Data.List (foldl')
import Data.Char (isLower, toUpper)
import Language.Haskell.TH

import Entity.Core


deriveStoreable :: Name -> Q [Dec]
deriveStoreable name =
    withType name (\tvbs cons ->  fromCons name  tvbs cons)

fromCons :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
fromCons _ _ [] = error "A Single Constructor is Required"
fromCons name tvbs [con] =
    let s = conE name
    in
     [d| data StoreField $(name) b = Test |]
    -- instanceD (return $ map (\t -> ClassP ''Storeable [VarT t]) typeNames)
    --           (classType `appT` instanceType)
    --           [dataInstD (return []) ''StoreField
    --            [ return (ConT $ mkName $ show name)
    --            , return (VarT $ mkName "typ")
    --            ]
    --            (withFieldsOf name con) []
    --           ]

             -- [ funD 'toJSON
              --   [ clause []
              --     (normalB $ consToJSON withField cons)
              --     []
              --   ]
              -- ]
  where
   classType = conT ''Storeable
   typeNames = map tvbName tvbs
   instanceType = foldl' appT (conT name) $ map varT typeNames
fromCons _ _ _ = error "Only 1 Constructor Allowed"

withFieldsOf :: Name -> Con -> [ConQ]
withFieldsOf store (RecC _ vars) =
    map buildCon  vars
  where
    buildCon (nm, _, typ) =
        let newName = mkName $ toCamelCase (nameBase nm)
        in
         return $ NormalC newName [ (IsStrict, ConT ''StoreField )
                                  , (IsStrict, ConT store)
                                  , (IsStrict, typ)
                                  ]
withFieldsOf _  _ = error "Only Record Allowed"

toCamelCase :: String -> String
toCamelCase xs = let (pres, sufs) = span isLower xs
                 in map toUpper pres ++ sufs

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name


withType :: Name -> ([TyVarBndr] ->[Con] -> Q [Dec])-> Q [Dec]
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f tvbs cons
          other -> error $ "Data.Aeson.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "Data.Aeson.TH.withType: I need the name of a type."
