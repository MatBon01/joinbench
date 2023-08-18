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
