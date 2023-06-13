class TableConfiguration():
    def __init__(self, table_name: str, num_records_in_table: int):
        self._table_name = table_name
        self._num_records_in_table = num_records_in_table

    @property
    def table_name(self) -> str:
        return self._table_name

    @property
    def num_records_in_table(self) -> int:
        return self._num_records_in_table
