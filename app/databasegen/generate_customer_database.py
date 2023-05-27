from random import Random
from typing import List

from tablegen.cells.amount_cell import AmountCell
from tablegen.cells.cell import Cell
from tablegen.cells.id_cell import IdCell
from tablegen.cells.random_choice_cell import RandomChoiceCell
from tablegen.cells.tracking_cell import TrackingCell
from tablegen.cells.unique_cell import UniqueCell
from tablegen.csv_table_generator import CSVTableGenerator
from tablegen.record_generator import RecordGenerator
from tablegen.table_generator import TableGenerator


def main() -> None:
    first_names: List[str] = ["Matteo", "John"]
    surnames: List[str] = ["Smith", "Jones"]
    random: Random = Random()
    generate_database(1000, 10000, first_names, surnames, random)


def generate_database(
    num_customer_records: int,
    num_order_records: int,
    first_names: List[str],
    surnames: List[str],
    random: Random,
    customer_table_name: str = "customers.csv",
    orders_table_name: str = "orders.csv",
) -> None:
    cids: List[str] = generate_customer_table(
        num_customer_records, first_names, surnames, random, customer_table_name
    )
    generate_order_table(num_order_records, cids, random, orders_table_name)


def generate_customer_table(
    num_records: int,
    first_names: List[str],
    surnames: List[str],
    random: Random,
    table_name: str = "customers.csv",
) -> List[str]:
    first_name_generator: Cell = RandomChoiceCell(first_names, random)
    surname_generator: Cell = RandomChoiceCell(surnames, random)
    id_generator: TrackingCell = TrackingCell(IdCell(random))

    record_generator: RecordGenerator = RecordGenerator(
        [first_name_generator, surname_generator, id_generator]
    )

    table_generator: TableGenerator = TableGenerator(record_generator)

    csv_table_generator: CSVTableGenerator = CSVTableGenerator(
        table_generator, table_name
    )
    csv_table_generator.generate(num_records)

    return list(id_generator.seen)


def generate_order_table(
    num_records: int,
    customer_ids: List[str],
    random: Random,
    table_name: str = "orders.csv",
) -> None:
    ids: Cell = UniqueCell(IdCell(random))
    cids: Cell = RandomChoiceCell(customer_ids, random)
    amount: Cell = AmountCell(random)


if __name__ == "__main__":
    main()
