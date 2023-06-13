from argparse import ArgumentParser
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


def main() -> None:
    parser: ArgumentParser = define_parser()

    customer_table_name: TableName
    customer_record_num: RecordNum
    invoice_table_name: TableName
    invoice_record_num: RecordNum
    first_names_source: str
    surnames_source: str

    (
        customer_table_name,
        customer_record_num,
        invoice_table_name,
        invoice_record_num,
        first_names_source,
        surnames_source,
    ) = parse_database_parameters(parser)

    first_names: List[str] = read_names(first_names_source)
    surnames: List[str] = read_names(surnames_source)
    random: Random = Random()
    generate_database(
        customer_record_num,
        invoice_record_num,
        first_names,
        surnames,
        random,
        customer_table_name,
        invoice_table_name,
    )


def define_parser() -> ArgumentParser:
    CUSTOMER_NUM_DEFAULT: RecordNum = 1000
    INVOICE_NUM_DEFAULT: RecordNum = 10000

    parser: ArgumentParser = ArgumentParser()
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
        "--invoice-records",
        help="number of invoice records",
        type=int,
        default=INVOICE_NUM_DEFAULT,
    )
    parser.add_argument(
        "--customer-table", help="customer table name", type=str, default="customers"
    )
    parser.add_argument(
        "--invoice-table", help="invoice table name", type=str, default="invoices"
    )
    parser.add_argument("firstnames", help="csv file with first names", type=str)
    parser.add_argument("surnames", help="csv file with surnames", type=str)
    parser.add_argument(
        "-d",
        "--add-date",
        help="add date to the file name",
        action="store_true",
    )

    return parser


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
    with open(filename, "r") as f:
        for row in f:
            res.append(row.strip().lower().capitalize())
    return res


def parse_database_parameters(
    parser: ArgumentParser,
) -> Tuple[TableName, RecordNum, TableName, RecordNum, str, str]:
    args = parser.parse_args()
    customer_record_num: RecordNum = args.customer_records
    invoice_record_num: RecordNum = args.invoice_records

    if customer_record_num < 0:
        raise ValueError("Number of records in the customer table must be nonnegative")
    if invoice_record_num < 0:
        raise ValueError("Number of records in the invoice table must be nonnegative")

    customer_table: TableName = combine_name(
        args.output, args.customer_table, args.add_date
    )
    invoice_table: TableName = combine_name(
        args.output, args.invoice_table, args.add_date
    )

    return (
        customer_table,
        customer_record_num,
        invoice_table,
        invoice_record_num,
        args.firstnames,
        args.surnames,
    )


def generate_database(
    num_customer_records: int,
    num_invoice_records: int,
    first_names: List[str],
    surnames: List[str],
    random: Random,
    customer_table_name: str = "customers.csv",
    invoice_table_name: str = "invoices.csv",
) -> None:
    cids: List[str] = generate_customer_table(
        num_customer_records, first_names, surnames, random, customer_table_name
    )
    generate_invoice_table(num_invoice_records, cids, random, invoice_table_name)


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


def generate_invoice_table(
    num_records: int,
    customer_ids: List[str],
    random: Random,
    table_name: str = "invoice.csv",
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
