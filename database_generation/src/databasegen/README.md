# databasegen package
This package contains the code used to create a synthetic database used in benchmarking the database system in question for this research paper.

For more details on the design principles and best-practices methodology, consult the final report in the releases.

## Contents
- `tablegen/`: a subpackage that allows the user to use low-level specifications to create a general synthetic database.
- `generate_JOINBENCH_database.py`: the script that created a synthetic dataset for the JOINBENCH schema.
- `configuration/`: a package containing classes that contain configuration data about the database tables to be created (used in the `generate_customer_database.py` script).
- `generate_customer_database.py`: a script that produces a more realistic database that was a first attempt at testing the database system that is the subject of this project.
- `first-names.csv` and `surnames.csv`: information to be used by the `generate_customer_database.py` script; legal and ethical considerations can be found in the report mentioned above.
