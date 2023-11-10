# JOINBENCH: Designing Tools for the Evaluation of Efficient Equijoins in Haskell
In order to evaluate alternative equijoin implementation in Haskell, this paper introduced the JOINBENCH relation and surrounding methodology and tooling. The JOINBENCH relation is a scheme designed for the synthesis of data optimised to help evaluate the performance of equijoins in a variety of scenarios. Furthermore, this paper presents a low-level library that helps users customise and define their own synthetic data sources for future benchmarking purposes.

In their distinguished paper "Relational Algebra by way of Adjunctions" it was noted that the monadic structure of bulk types can help explain most of relational algebra. Using this structure, the authors designed a new method to facilitate the use of monad comprehensions in an efficient implementation of equijoins of relational databases. This project presents an implementation of such a system and an evaluation of the performance such query optimisations carry using the bespoke tooling described above. 

## Repository outline
- Database system library see: `src/`, `test/` (requires environment as described by `joinbench.cabal`).
- Database benchmarking client code: `app/`, `test/` (requires environment as described by `joinbench.cabal`).
- Synthetic database generation modules: `database_generation/`.
- Data visualisation and analysis modules: `analysis/` (requires environment set up using `conda-env.txt`).
- Report: `report/`.
- Various automations: see `Makefile`s in directories and `.github/` for CI/CD pipeline.
