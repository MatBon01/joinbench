.PHONY: joinbench

database_gen_path = database_generation/src/databasegen
tables_path = tables

joinbench: $(tables_path)/join_bench_table.csv
	cabal run joinbench-benchmark

$(tables_path)/join_bench_table.csv:
	python3 $(database_gen_path)/generate_JOINBENCH_database.py 50000 ${tables_path}/join_bench_table.csv
	cd database_generation

invoice100:
	cabal run invoice-benchmark app/examples/tables/c100.csv app/examples/tables/i100.csv
