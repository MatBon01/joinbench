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
