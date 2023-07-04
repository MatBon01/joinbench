from random import Random
from typing import List

from tablegen.cells.cell import Cell
from tablegen.cells.counter_cell import CounterCell
from tablegen.cells.even_integer_cell import EvenIntegerCell
from tablegen.cells.odd_integer_cell import OddIntegerCell
from tablegen.cells.random_modular_integer_cell import RandomModularIntegerCell
from tablegen.csv_table_generator import CSVTableGenerator
from tablegen.record_generator import RecordGenerator
from tablegen.table_generator import TableGenerator


def main():
    OUTPUT_FILE_NAME: str = "benchmarking_database.csv"
    NUM_RECORDS: int = 1000
    generate_database(NUM_RECORDS, OUTPUT_FILE_NAME)


def generate_database(num_records: int, file_path: str) -> None:
    random: Random = Random()
    record_generator: RecordGenerator = construct_record_generator(random)
    table_generator: TableGenerator = TableGenerator(record_generator)
    csv_table_generator: CSVTableGenerator = CSVTableGenerator(
        table_generator, file_path
    )
    csv_table_generator.generate(num_records)


def construct_record_generator(random: Random) -> RecordGenerator:
    cells: List[Cell] = get_cell_list(random)

    return RecordGenerator(cells)


def get_cell_list(random: Random) -> List[Cell]:
    id: Cell = CounterCell()
    one_percent: Cell = RandomModularIntegerCell(100, random)
    twenty_percent: Cell = RandomModularIntegerCell(5, random)
    twenty_five_percent: Cell = RandomModularIntegerCell(4, random)
    fifty_percent: Cell = RandomModularIntegerCell(2, random)
    even_one_percent: Cell = EvenIntegerCell(100, random)
    odd_one_percent: Cell = OddIntegerCell(100, random)

    cells: List[Cell] = [
        id,
        one_percent,
        twenty_percent,
        twenty_five_percent,
        fifty_percent,
        even_one_percent,
        odd_one_percent,
    ]
    return cells


if __name__ == "__main__":
    main()
