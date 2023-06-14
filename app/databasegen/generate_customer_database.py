from argparse import ArgumentParser, Namespace
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
from tablegen.table_configuration import TableConfiguration
from tablegen.table_generator import TableGenerator

TableName = str
RecordNum = int


def main() -> None:
    parser: ArgumentParser = define_parser()

    first_names_source: str
    surnames_source: str

    (
        customer_table_config,
        invoice_table_config,
        first_names_source,
        surnames_source,
    ) = parse_database_parameters(parser)

    first_names: List[str] = read_names(first_names_source)
    surnames: List[str] = read_names(surnames_source)

    random: Random = Random()
    generate_database(
        customer_table_config,
        invoice_table_config,
        first_names,
        surnames,
        random,
    )


def define_parser() -> ArgumentParser:
    parser: ArgumentParser = ArgumentParser()

    parser = add_customer_table_arguments_to_parser(parser)
    parser = add_invoice_table_arguments_to_parser(parser)
    parser = add_output_arguments_to_parser(parser)

    return parser


def add_customer_table_arguments_to_parser(parser: ArgumentParser) -> ArgumentParser:
    CUSTOMER_NUM_DEFAULT: RecordNum = 1000
    parser.add_argument(
        "--customer-records",
        help="number of customer records",
        type=int,
        default=CUSTOMER_NUM_DEFAULT,
    )
    parser.add_argument(
        "--customer-table", help="customer table name", type=str, default="customers"
    )
    parser.add_argument("firstnames", help="csv file with first names", type=str)
    parser.add_argument("surnames", help="csv file with surnames", type=str)
    return parser


def add_invoice_table_arguments_to_parser(parser: ArgumentParser) -> ArgumentParser:
    INVOICE_NUM_DEFAULT: RecordNum = 10000
    parser.add_argument(
        "--invoice-records",
        help="number of invoice records",
        type=int,
        default=INVOICE_NUM_DEFAULT,
    )
    parser.add_argument(
        "--invoice-table", help="invoice table name", type=str, default="invoices"
    )
    return parser


def add_output_arguments_to_parser(parser: ArgumentParser) -> ArgumentParser:
    parser.add_argument(
        "-o", "--output", help="output for the files", type=str, default="."
    )
    parser.add_argument(
        "-d",
        "--add-date",
        help="add date to the file name",
        action="store_true",
    )
    return parser


def parse_database_parameters(
    parser: ArgumentParser,
) -> Tuple[TableConfiguration, TableConfiguration, str, str]:
    args: Namespace = parser.parse_args()

    customer_table_config: TableConfiguration = parse_customer_table_arguments(args)
    invoice_table_config: TableConfiguration = parse_invoice_table_arguments(args)

    return (customer_table_config, invoice_table_config, args.firstnames, args.surnames)


def parse_invoice_table_arguments(args: Namespace) -> TableConfiguration:
    invoice_record_num: RecordNum = args.invoice_records
    if invoice_record_num < 0:
        raise ValueError("Number of records in the invoice table must be nonnegative")
    invoice_table: TableName = combine_name(
        args.output, args.invoice_table, args.add_date
    )
    return TableConfiguration(invoice_table, invoice_record_num)


def parse_customer_table_arguments(args: Namespace) -> TableConfiguration:
    customer_record_num: RecordNum = args.customer_records
    if customer_record_num < 0:
        raise ValueError("Number of records in the customer table must be nonnegative")
    customer_table: TableName = combine_name(
        args.output, args.customer_table, args.add_date
    )
    return TableConfiguration(customer_table, customer_record_num)


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


def generate_database(
    customer_table_config: TableConfiguration,
    invoice_table_config: TableConfiguration,
    first_names: List[str],
    surnames: List[str],
    random: Random,
) -> None:
    cids: List[str] = generate_customer_table(
        customer_table_config.num_records_in_table,
        first_names,
        surnames,
        random,
        customer_table_config.table_name,
    )
    generate_invoice_table(
        invoice_table_config.num_records_in_table,
        cids,
        random,
        invoice_table_config.table_name,
    )


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
