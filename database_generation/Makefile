module_path = src/databasegen
tables_path = ../tables/
example_path = ../app/examples/

joinbench:
	python3 $(module_path)/generate_JOINBENCH_database.py 50000 ${tables_path}/join_bench_table.csv

hundred:
	python3 $(module_path)/generate_customer_database.py -o ${example_path}/tables --customer-records 100 --invoice-records 100 --customer-table c100 --invoice-table i100 $(module_path)/first-names.csv $(module_path)/surnames.csv
