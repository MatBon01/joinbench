class TableConfiguration():
    @property
    def table_name(self) -> str:
        raise NotImplementedError("Must be implemented by subclass")

    @property
    def num_records_in_table(self) -> int:
        raise NotImplementedError("Must be implemented by subclass")
