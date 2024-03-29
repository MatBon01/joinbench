%include polycode.fmt

\begin{code}
productEquijoin :: (Eq c) => (a -> c) -> (b -> c) -> (Bag a, Bag b) -> Bag (a, b)
productEquijoin fa fb = select equality . cp
  where
    equality (a, b) = fa a == fb b
\end{code}

\begin{code}
cp :: (Bag a, Bag b) -> Bag (a, b)
\end{code}

\begin{code}
select :: (a -> Bool) -> Bag a -> Bag a
\end{code}

\begin{code}
comprehensionEquijoin :: (Eq c) => (a -> c) -> (b -> c) -> (Bag a, Bag b) ->
Bag (a, b)
comprehensionEquijoin fa fb (as, bs) = [(a, b) | a <- as, b <- bs, fa a == fb b]
\end{code}

\begin{code}
indexedEquijoin :: (Key k) => (a -> k) -> (b -> k) -> (Bag a, Bag b) -> Bag (a, b)
indexedEquijoin if1 if2 (t1, t2) = (reduce . fmap cp . merge) (it1, it2)
  where
    it1 = t1 `indexBy` if1
    it2 = t2 `indexBy` if2
\end{code}

\begin{code}
indexBy  ::  (Key k) => Table a -> (a -> k) -> Map k (Table a)
merge    ::  (Map k v1, Map k v2) -> Map k (v1, v2)
reduce   ::  (PointedSet v, CMonoid v) => Map k v -> v
\end{code}

\begin{code}
instance (NFData a) => NFData (Bag a) where
    rnf (Bag xs) = rnf xs

instance NFData JoinBenchRecord where
    rnf a =
        rnf
            ( uid a
            , onePercent a
            , twentyPercent a
            , twentyfivePercent a
            , fiftyPercent a
            , evenOnePercent a
            , oddOnePercent a
            )

\end{code}

\begin{code}
newtype Bag a = Bag {elements :: [a]}

instance (Eq a) => Eq (Bag a) where
    b1 == b2 = eq' (elements b1) (elements b2)
      where
        eq' ::  (Eq a) => [a] -> [a] -> Bool
        eq' (x : xs) ys  =  not (null ys2) && eq' xs (ys1 ++ tail ys2)
          where
            (ys1, ys2) = break (== x) ys
        eq' [] []        =  True
        eq' _ _          =  False
\end{code}

\begin{code}
class PointedSet a where
    null    ::  a
    isNull  ::  a -> Bool
\end{code}

\begin{code}
instance PointedSet (Bag a) where
    null             =  Bag.empty
    isNull (Bag [])  =  True
    isNull _         =  False

instance PointedSet (Maybe a) where
    null    =  Nothing
    isNull  =  isNothing
\end{code}

\begin{code} 
instance Key Word16 where -- constant type (array indexed by 16 bit word)
    newtype Map Word16 v  =  A (Array Word16 v) deriving (Eq, Show)
    empty                 =  A (accumArray (\_ x -> x) PointedSet.null (0, 2 ^ 16 - 1) [])
    isEmpty (A a)         =  all isNull (elems a)
    single (k, v)         =  A (accumArray (\_ x -> x) PointedSet.null (0, 2 ^ 16 - 1) [(k, v)])
    merge (A a1, A a2)    =  A (listArray (0, 2 ^ 16 - 1) (zip (elems a1) (elems a2)))
    dom (A a)             =  Bag [k | (k, v) <- assocs a, not (isNull v)]
    cod (A a)             =  Bag [v | (k, v) <- assocs a, not (isNull v)]
    lookup (A a)          =  (!) a
    index kvps            =  A (accumArray (curry Bag.union) Bag.empty (0, 2 ^ 16 - 1) vals)
      where
        vals              =  (elements . fmap (Bifunctor.second Bag.single)) kvps
    unindex (A a)         =  Bag [(k, v) | (k, vs) <- assocs a, v <- elements vs]
\end{code}

\begin{code}
data Students  =  S {uid :: Int, name :: String, age :: Int}
data Grades    =  G {sid :: Int, subject :: String, grade :: Char}
\end{code}

\begin{code}
merge (students `indexBy` uid, grades `indexBy` sid)
\end{code}

\begin{code}
(reduce . fmap cp) mergedStudentsAndGrades
\end{code}

\begin{code}
indexBy  ::  (Key k) => Bag a -> (a -> k) -> Map k (Bag a)
cp       ::  (Bag v1, Bag v2) -> Bag (v1, v2)
merge    ::  (Map k (Bag v1), Map k (Bag v2)) -> Map k (Bag v1, Bag v2)
reduce   ::  Map k (Bag v1, Bag v2) -> Bag (v1, v2)
\end{code}

\begin{code}
newtype Bag a = Bag {elements :: [a]}
\end{code}

\begin{code}
(reduce . fmap cp . merge) (istudents, igrades)
    where
        istudents = students `indexBy` uid
        igrades = grades `indexBy` sid
\end{code}
