.SECONDARY:
.PHONY: joinbench

database_gen_path = database_generation/src/databasegen
tables_path = tables
data_path = data

joinbench: joinbench100

joinbenchwhnf%: $(tables_path)/joinbenchtable%.csv
	cabal run whnf-joinbench-benchmark -- $(tables_path)/joinbenchtable$*.csv --template json --output data/joinbenchwhnf$*.json

joinbench%: $(tables_path)/joinbenchtable%.csv
	cabal run joinbench-benchmark -- $(tables_path)/joinbenchtable$*.csv --template json --output data/joinbench$*.json

$(tables_path)/joinbenchtable%.csv:
	python3 $(database_gen_path)/generate_JOINBENCH_database.py $* ${tables_path}/joinbenchtable$*.csv

invoice100:
	cabal run invoice-benchmark app/examples/tables/c100.csv app/examples/tables/i100.csv
