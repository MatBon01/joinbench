import json
from collections import defaultdict
from typing import Dict, List, Set, Tuple


class BenchmarkOrganiser:
    def __init__(self, data_path: str):
        with open(data_path, "r") as f:
            self.data = json.load(f)

    def get_benchmark_group_names(self) -> List[str]:
        return list(BenchmarkOrganiser._get_report_names((self.data)))

    @staticmethod
    def _get_report_names(data) -> Set[str]:
        names: Set[str] = set()
        for group in data:
            names.add(group["reportName"])
        return names

    @staticmethod
    def separate_benchmark_group_and_name(reportName: str) -> Tuple[str, str]:
        groupBenchmarkNames = reportName.split("/")
        return groupBenchmarkNames[0], groupBenchmarkNames[1]

    @staticmethod
    def map_benchmark_groups_and_benchmark_indices(
        names,
    ) -> Dict[str, List[Tuple[int, str]]]:
        groups: Dict[str, List[Tuple[int, str]]] = defaultdict(list)
        for i, name in enumerate(names):
            group: str
            name: str
            group, name = BenchmarkOrganiser.separate_benchmark_group_and_name(name)

            groups[group].append((i, name))

        return groups
