module_path = src/databasegen
output_path = ../app/examples

hundred:
	python3 $(module_path)/generate_customer_database.py -o ${output_path}/tables --customer-records 100 --invoice-records 100 --customer-table c100 --invoice-table i100 $(module_path)/first-names.csv $(module_path)/surnames.csv
