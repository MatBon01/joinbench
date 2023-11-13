# Analysis
This part of the project analyses the benchmarked results.

The role of this part of the project is to automatically organise, analyse and visualise the results from the experiments.
It contains a package `src/joinbench` whose modules organise and visualise the data in different ways. This design was chosen so that the method of generation and analysis could change independent to the logic. For instance, the scripts used in the Jupyter notebook could easily be exported and run in a pipeline to generate the figures used in the report automatically. I preferred the manual approach so I could check all figures in my report before compilation.

## Contents
- `src/joinbench`: contains packages that visualise and organise the results.
- `joinbench.ipynb`: a way to manually run the code in the joinbench package in order to see the results and create assets for the report.
