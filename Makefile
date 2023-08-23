.SECONDARY:
.PHONY: joinbench

database_gen_path = database_generation/src/databasegen
tables_path = tables
data_path = data

benchmark:
	make joinbench10
	make joinbench25
	make joinbench50
	make joinbench100
	make joinbench250
	make joinbench500
	make joinbench750
	make joinbench1000
	make joinbench1000
	make joinbench2000
	make joinbench3000
	make joinbench4000
	make joinbench5000
	make joinbench6000
	make joinbench7000
	make joinbench8000
	make joinbench9000
	make joinbench10000
	make joinbenchwhnf1000

joinbench: joinbench100

joinbenchwhnf%: $(tables_path)/joinbenchtable%.csv
	cabal run whnf-joinbench-benchmark -- $(tables_path)/joinbenchtable$*.csv --template json --output data/whnf/joinbench$*.json

joinbench%: $(tables_path)/joinbenchtable%.csv
	cabal run joinbench-benchmark -- $(tables_path)/joinbenchtable$*.csv --template json --output data/joinbench$*.json

$(tables_path)/joinbenchtable%.csv:
	python3 $(database_gen_path)/generate_JOINBENCH_database.py $* ${tables_path}/joinbenchtable$*.csv

invoice100:
	cabal run invoice-benchmark app/examples/tables/c100.csv app/examples/tables/i100.csv
