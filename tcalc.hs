{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{- HLINT ignore "Use if" -}

data Typ where
    TypVal :: String -> Typ
    TypFun :: Typ -> Typ -> Typ
    deriving (Eq)

data Var = Var
    { name :: String
    , free :: Bool }
    deriving (Eq)

data VarDecl = VarDecl
    { var :: Var
    , typ :: Typ }
    deriving (Eq)

mkDecl :: String -> Typ -> VarDecl
mkDecl n t = VarDecl { var = Var { name = n, free = True } , typ = t }

data Expr where
    ExprVar      :: Var  -> Expr
    ExprFunApp   :: Expr -> Expr -> Expr
    ExprLambda   :: Var  -> Expr -> Expr
    deriving (Eq)


findTyp :: Typ -> Typ -> Maybe [Typ]
findTyp t            c | t == c = Just []
findTyp t (TypFun a b) | t == b = Just [a]
findTyp t (TypFun a b)          = fmap (a :) (findTyp t b)
findTyp _            _          = Nothing



findTypInVars :: Typ -> [VarDecl] -> [(Var, [Typ])]
findTypInVars t [] = []
findTypInVars t (VarDecl vName vType : vs) =
    let vRes = case findTyp t vType of
            Nothing -> []
            Just ts -> [(vName, ts)]
    in
        vRes ++ findTypInVars t vs


concatFunExprs :: Expr -> [Expr] -> Expr
concatFunExprs = foldl ExprFunApp


newtype VarGen = VarMaker Char

mkVarGen :: VarGen
mkVarGen = VarMaker '₀'

mkVar :: VarGen -> Typ -> (VarDecl, VarGen)
mkVar (VarMaker ch) t = (
        VarDecl { var = Var { name = [ch], free = False } , typ = t },
        VarMaker (succ ch)
    )



maxDepth :: Int
maxDepth = 10


findExprs :: [VarDecl] -> Typ -> [Expr]
findExprs vs t =
    let
        go :: Int -> VarGen -> [VarDecl] -> Typ -> [Expr]
        go depth varGen vs t =
            let
                findDirect :: [Expr]
                findDirect = do
                    (varName, typs) <- findTypInVars t vs
                    let subs = fmap (go (depth+1) varGen vs) typs
                    let sc = concatFunExprs (ExprVar varName) <$> sequence subs
                    sc
                
                findLambda :: [Expr]
                findLambda = 
                    case t of
                        (TypFun l r) ->
                            let
                                newDecl :: VarDecl
                                varGen' :: VarGen
                                (newDecl, varGen') = mkVar varGen l
                                vs' = newDecl : vs
                                exprs = go (depth+1) varGen' vs' r
                                mkLambda :: Expr -> Expr
                                mkLambda = ExprLambda (var newDecl)
                            in
                                fmap mkLambda exprs
                        _            -> []

            in
                if depth > maxDepth then [] else
                findDirect ++ findLambda
    in
        take 1 $
        go 0 mkVarGen vs t



typA, typB, typC, typD :: Typ
typA = TypVal "a"
typB = TypVal "b"
typC = TypVal "c"
typD = TypVal "d"
typR = TypVal "r"

res :: [Expr]
res = findExprs vars typRes


vars :: [VarDecl]
vars = [
          mkDecl "f" (TypFun     typA    (TypFun  (TypFun typB typR)  typR)),   -- f :: a -> (b -> r) -> r
          mkDecl "m" (TypFun  (TypFun typA typR)  typR)                         -- m :: (a -> r) -> r
       ]
typRes :: Typ
typRes = TypFun  (TypFun typB typR)  typR                                       -- ? :: (b -> r) -> r      = \a -> m (\b -> f b a)



main :: IO ()
main =
    mapM_ print res




instance Show Typ where
    show (TypVal n) = n
    show (TypFun (TypFun ll lr) r) = "(" ++ show ll ++ " -> " ++ show lr ++ ") -> " ++ show r
    show (TypFun l r) = show l ++ " -> " ++ show r

instance Show Var where
    show Var{ name, free } = case free of
        True  ->        name
        False -> "t" ++ name

instance Show VarDecl where
    show VarDecl{ var, typ } = show var ++ " :: " ++ show typ

instance Show Expr where
    show (ExprVar v) = show v
    show (ExprFunApp l (ExprFunApp rl rr)) = show l ++ " (" ++ show rl ++ " " ++ show rr ++ ")"
    show (ExprFunApp l r) = show l ++ " " ++ show r
    show (ExprLambda b e) = "\\" ++ show b ++ " -> " ++ show e
