from .benchmark_group import BenchmarkGroup


class BenchmarkGroupPlotter:
    def __init__(self, group: BenchmarkGroup):
        self.group: BenchmarkGroup = group
