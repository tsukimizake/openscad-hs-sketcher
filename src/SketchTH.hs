module SketchTH (mkSketchRes) where

import Data.Function ((&))
import Data.Proxy
import Language.Haskell.TH
import OpenSCAD
import SketchTypes

data Field = SkPolygon Name | SkPoint Name deriving (Show, Eq)

data SketchRecord = Record
  { origname :: Name,
    fields :: [Field]
  }
  deriving (Show, Eq)

-- | Generate Result Record from Sketch Record
mkResRecord :: SketchRecord -> Q Dec
mkResRecord (Record origName _) = do
  let resName = mkResRecordName origName
  getFields origName >>= \case
    recfields -> do
      let resFields = fmap (\(n, t) -> (toBaseName n, Bang Language.Haskell.TH.NoSourceUnpackedness Language.Haskell.TH.NoSourceStrictness, getResType t)) recfields
      pure $ DataD [] resName [] Nothing [RecC resName resFields] [DerivClause Nothing [ConT ''Show]]

getFields :: Name -> Q [(Name, Type)]
getFields name = do
  TyConI (DataD _ _ _ _ constructors _) <- reify name
  case constructors of
    [RecC _ recfields] -> pure $ fmap (\(n, _, t) -> (n, t)) recfields
    _ -> error "not a record"

getResType :: Type -> Type
getResType (ConT name)
  | name == ''SketchTypes.Polygon = ConT ''OpenSCAD.Model2d
  | name == ''SketchTypes.Point = ConT ''OpenSCAD.Vector2d
  | otherwise = ConT name
getResType _ = error "not a ConT"

-- | generate SketchRecord from Name
readRecord :: Name -> Q SketchRecord
readRecord name = do
  TyConI (DataD _ _ _ _ constructors _) <- reify name
  case constructors of
    [RecC _ recfields] -> do
      let fields =
            fmap
              ( \(n, _, t) -> case t of
                  ConT typename
                    | typename == ''SketchTypes.Polygon -> SkPolygon $ toBaseName n
                    | typename == ''SketchTypes.Point -> SkPoint $ toBaseName n
                  _ -> error $ "invalid type in record field" ++ show t
              )
              recfields
      pure $ Record name fields
    _ -> error "not a record"

-- | generate toList :: a -> ([Sketch], Proxy a)
generateToList :: SketchRecord -> Q Dec
generateToList (Record origname fields) = do
  let recname = mkName "a"
  pure $
    FunD
      (mkName "toList")
      [ Clause
          [VarP recname]
          ( NormalB $
              TupE
                [ Just
                    ( ListE
                        ( fields
                            & fmap
                              ( \case
                                  SkPolygon n -> AppE (VarE 'wrapShape) (GetFieldE (VarE recname) (nameBase n))
                                  SkPoint n -> AppE (VarE 'wrapShape) (GetFieldE (VarE recname) (nameBase n))
                              )
                        )
                    ),
                  Just (AppTypeE (ConE 'Proxy) (ConT origname))
                ]
          )
          []
      ]

-- | generate fromList :: ([Sketch], Proxy a) -> ResRecord a
generateFromList :: SketchRecord -> Q Dec
generateFromList (Record origname fields) = do
  pure $
    FunD
      (mkName "fromList")
      [ Clause
          [TupP [ListP (fields & fmap (\case SkPolygon n -> VarP n; SkPoint n -> VarP n)), VarP $ mkName "_proxy"]]
          ( NormalB $
              RecConE
                (mkResRecordName origname)
                ( fields
                    & fmap
                      ( \case
                          SkPolygon n -> (n, AppE (VarE 'unwrapModelRes) (VarE n))
                          SkPoint n -> (n, AppE (VarE 'unwrapPointRes) (VarE n))
                      )
                )
          )
          []
      ]

-- data Hoge = Hoge
--   { honi :: Polygon,
--     fuwa :: Point,
--     poyo :: Polygon
--   }
--   deriving (Show)
--
-- mkSketchRes ''Hoge
--
-- makes:
--
-- data HogeRes
--   = HogeRes
--   { honi :: OpenSCAD.Model2d,
--     fuwa :: OpenSCAD.Vector2d,
--     poyo :: OpenSCAD.Model2d
--   }
--   deriving (Show)
--
-- instance SketchTypes.Models SketchTry.Hoge where
--   type Res SketchTry.Hoge = HogeRes
--   toList a =
--     ( [ SketchTypes.wrapShape a.honi,
--         SketchTypes.wrapShape a.fuwa,
--         SketchTypes.wrapShape a.poyo
--       ],
--       Data.Proxy.Proxy @SketchTry.Hoge
--     )
--   fromList ([honi, fuwa, poyo], _proxy) =
--     HogeRes
--       { honi = SketchTypes.unwrapModelRes honi,
--         fuwa = SketchTypes.unwrapPointRes fuwa,
--         poyo = SketchTypes.unwrapModelRes poyo
--       }

mkSketchRes :: Name -> Q [Dec]
mkSketchRes recordname =
  do
    record <- readRecord recordname
    resRecordDecl <- mkResRecord record
    toList' <- generateToList record
    fromList' <- generateFromList record
    let res =
          [ resRecordDecl,
            InstanceD
              Nothing
              []
              (AppT (ConT ''Models) (ConT recordname))
              [ TySynInstD $ TySynEqn Nothing (AppT (ConT $ toBaseName ''Res) (ConT recordname)) (ConT $ mkResRecordName recordname),
                toList',
                fromList'
              ]
          ]
    -- runIO $ putStrLn $ pprint res
    pure res

-- helpers

mkResRecordName :: Name -> Name
mkResRecordName name = mkName $ nameBase name ++ "Res"

toBaseName :: Name -> Name
toBaseName = mkName . nameBase
