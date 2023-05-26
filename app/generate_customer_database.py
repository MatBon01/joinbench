from typing import List

from tablegen.cells.cell import Cell
from tablegen.cells.id_cell import IdCell
from tablegen.cells.random_choice_cell import RandomChoiceCell
from tablegen.cells.tracking_cell import TrackingCell
from tablegen.csv_table_generator import CSVTableGenerator
from tablegen.record_generator import RecordGenerator
from tablegen.table_generator import TableGenerator


def main() -> None:
    first_names: List[str] = ["Matteo", "John"]
    surnames: List[str] = ["Smith", "Jones"]
    generate_database(1000, 10000, first_names, surnames)


def generate_database(
    num_customer_records: int,
    num_order_records: int,
    first_names: List[str],
    surnames: List[str],
    customer_table_name: str = "customers.csv",
    orders_table_name: str = "orders.csv",
) -> None:
    cids: List[str] = generate_customer_table(
        num_customer_records, first_names, surnames, customer_table_name
    )
    generate_order_table(num_order_records, cids, orders_table_name)


def generate_customer_table(
    num_records: int,
    first_names: List[str],
    surnames: List[str],
    table_name: str = "customers.csv",
) -> List[str]:
    first_name_generator: Cell = RandomChoiceCell(first_names)
    surname_generator: Cell = RandomChoiceCell(surnames)
    id_generator: TrackingCell = TrackingCell(IdCell())

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
    num_records: int, customer_ids: List[str], table_name: str = "orders.csv"
) -> None:
    pass


if __name__ == "__main__":
    main()
