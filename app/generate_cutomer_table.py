from tablegen.table_generator import TableGenerator
from tablegen.record_generator import RecordGenerator
from tablegen.cells.id_cell import IdCell
from tablegen.cells.duplicated_cell import DuplicateCell
from tablegen.cells.random_name_cell import RandomNameCell
from tablegen.cells.cell import Cell

def main() -> None:
    name_generator: Cell = RandomNameCell(["Matteo", "Bina", "Alice", "Bob"])
    id_generator: Cell  = DuplicateCell(IdCell(), 0.5)

    record_generator: RecordGenerator = RecordGenerator([name_generator, id_generator])

    table_generator: TableGenerator = TableGenerator(record_generator)

    print(list(table_generator.generate(10)))

if __name__ == "__main__":
    main()
