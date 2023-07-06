.SECONDARY:
.PHONY: joinbench

database_gen_path = database_generation/src/databasegen
tables_path = tables

joinbench: joinbench100

joinbench%: $(tables_path)/join_bench_table_%.csv
	cabal run joinbench-benchmark $(tables_path)/join_bench_table_$*.csv

$(tables_path)/join_bench_table_%.csv:
	python3 $(database_gen_path)/generate_JOINBENCH_database.py $* ${tables_path}/join_bench_table_$*.csv

invoice100:
	cabal run invoice-benchmark app/examples/tables/c100.csv app/examples/tables/i100.csv
