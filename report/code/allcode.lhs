%include polycode.fmt

\begin{newcode}
productEquijoin :: (Eq c) => (a -> c) -> (b -> c) -> (Table a, Table b) -> Table (a, b)
productEquijoin fa fb = select equality . cp
  where
    equality (a, b) = fa a == fb b
\end{newcode}
