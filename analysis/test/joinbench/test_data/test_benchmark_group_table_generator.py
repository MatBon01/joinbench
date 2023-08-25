from test_utils.benchmark_utils import get_joinbench_benchmark_group

from joinbench.benchmark_group import BenchmarkGroup
from joinbench.benchmark_group_table_generator import \
    BenchmarkGroupTableGenerator


class TestBenchmarkGroupTableGenerator:
    def test_can_create_table_generator_with_benchmark_group(self):
        bg: BenchmarkGroup = get_joinbench_benchmark_group()
        assert BenchmarkGroupTableGenerator(bg)

    def test_can_calucate_percentage_change(self):
        assert BenchmarkGroupTableGenerator.calculate_percentage_change(1, 2) == 100
