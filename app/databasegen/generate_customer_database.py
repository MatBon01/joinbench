import argparse
from datetime import datetime
from random import Random
from typing import List, Tuple

from tablegen.cells.amount_cell import AmountCell
from tablegen.cells.cell import Cell
from tablegen.cells.date_cell import DateCell
from tablegen.cells.id_cell import IdCell
from tablegen.cells.random_choice_cell import RandomChoiceCell
from tablegen.cells.tracking_cell import TrackingCell
from tablegen.cells.unique_cell import UniqueCell
from tablegen.csv_table_generator import CSVTableGenerator
from tablegen.record_generator import RecordGenerator
from tablegen.table_generator import TableGenerator

TableName = str
RecordNum = int


def combine_name(
    output: str, name: str, add_date: bool, extension: str = ".csv"
) -> TableName:
    # assumes that the name does not have an extension on it
    res: str = output + "/" + name
    if add_date:
        res += "_" + datetime.now().strftime("%y%m%d%H%M%S")
    res += extension
    return res

def read_names(filename: str) -> List[str]:
    res: List[str] = []
    with open(filename, 'r') as f:
        for row in f:
            res.append(row.strip().lower().capitalize())
    return res

def parse_database_parameters() -> (
    Tuple[TableName, RecordNum, TableName, RecordNum, str, str]
):
    CUSTOMER_NUM_DEFAULT: RecordNum = 1000
    ORDER_NUM_DEFAULT: RecordNum = 10000
    parser: argparse.ArgumentParser = argparse.ArgumentParser()
    parser.add_argument(
        "-o", "--output", help="output for the files", type=str, default="."
    )
    parser.add_argument(
        "--customer-records",
        help="number of customer records",
        type=int,
        default=CUSTOMER_NUM_DEFAULT,
    )
    parser.add_argument(
        "--order-records",
        help="number of order records",
        type=int,
        default=ORDER_NUM_DEFAULT,
    )
    parser.add_argument(
        "--customer-table", help="customer table name", type=str, default="customers"
    )
    parser.add_argument(
        "--order-table", help="order table name", type=str, default="orders"
    )
    parser.add_argument("firstnames", help="csv file with first names", type=str)
    parser.add_argument("surnames", help="csv file with surnames", type=str)
    parser.add_argument(
        "-d",
        "--add-date",
        help="add date to the file name",
        action="store_true",
        default=True,
    )

    args = parser.parse_args()

    customer_record_num: RecordNum = args.customer_records
    order_record_num: RecordNum = args.order_records

    if customer_record_num < 0:
        customer_record_num = CUSTOMER_NUM_DEFAULT
    if order_record_num < 0:
        order_record_num = ORDER_NUM_DEFAULT

    customer_table: TableName = combine_name(
        args.output, args.customer_table, args.add_date
    )
    order_table: TableName = combine_name(args.output, args.order_table, args.add_date)

    return (
        customer_table,
        customer_record_num,
        order_table,
        order_record_num,
        args.firstnames,
        args.surnames,
    )


def main() -> None:
    customer_table_name: TableName
    customer_record_num: RecordNum
    order_table_name: TableName
    order_record_num: RecordNum
    first_names_source: str
    surnames_source: str

    (
        customer_table_name,
        customer_record_num,
        order_table_name,
        order_record_num,
        first_names_source,
        surnames_source,
    ) = parse_database_parameters()

    first_names: List[str] = read_names(first_names_source)
    surnames: List[str] = read_names(surnames_source)
    random: Random = Random()
    generate_database(customer_record_num, order_record_num, first_names,
                      surnames, random, customer_table_name, order_table_name)


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
    id: Cell = UniqueCell(IdCell(random))
    cid: Cell = RandomChoiceCell(customer_ids, random)
    due: Cell = DateCell(1990, 2023, random)
    amount: Cell = AmountCell(random)

    record_generator: RecordGenerator = RecordGenerator([id, cid, due, amount])
    table_generator: TableGenerator = TableGenerator(record_generator)
    csv_table_generator: CSVTableGenerator = CSVTableGenerator(
        table_generator, table_name
    )

    csv_table_generator.generate(num_records)


if __name__ == "__main__":
    main()
