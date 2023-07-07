from collections import defaultdict
from typing import Dict, List, Set, Tuple

import pandas as pd


class BenchmarkData:
    def __init__(self, data_path: str):
        self.data = pd.read_json(data_path)

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
            group, name = BenchmarkData.separate_benchmark_group_and_name(name)

            groups[group].append((i, name))

        return groups

    @staticmethod
    def get_benchmark_group_names(names) -> Set[str]:
        return set(
            BenchmarkData.map_benchmark_groups_and_benchmark_indices(names).keys()
        )
