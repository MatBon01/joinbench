from collections import defaultdict
from typing import Dict, List, Set, Tuple

import pandas as pd


class BenchmarkData:
    def __init__(self, data_path: str, number_of_tuples: int):
        self.data = pd.read_json(data_path)
        self.number_of_tuples: int = number_of_tuples

    def get_report_names_in_order(self) -> List[str]:
        return list(self.data["reportName"])

    @staticmethod
    def separate_benchmark_group_and_name(reportName: str) -> Tuple[str, str]:
        groupBenchmarkNames = reportName.split("/")
        return groupBenchmarkNames[0], groupBenchmarkNames[1]

    def map_benchmark_groups_and_benchmark_indices(
        self,
    ) -> Dict[str, List[Tuple[int, str]]]:
        names: List[str] = list(self.get_report_names_in_order())
        groups: Dict[str, List[Tuple[int, str]]] = defaultdict(list)
        for i, name in enumerate(names):
            group: str
            name: str
            group, name = BenchmarkData.separate_benchmark_group_and_name(name)

            groups[group].append((i, name))

        return groups

    def get_benchmark_group_names(self) -> Set[str]:
        return set(self.map_benchmark_groups_and_benchmark_indices().keys())

    def get_benchmark_mean(self, index: int) -> float:
        return self.data["reportAnalysis"][index]["anMean"]["estPoint"]

    def get_means_of_benchmark_list(self, indices: List[int]) -> List[float]:
        return list(map(self.get_benchmark_mean, indices))
