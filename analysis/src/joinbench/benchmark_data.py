from collections import defaultdict
from typing import Dict, List, Set, Tuple

import pandas as pd


class BenchmarkData:
    def __init__(self, data_path: str, tuple_count: int):
        self.data = pd.read_json(data_path)
        self.tuple_count: int = tuple_count

    def get_report_names_in_order(self) -> List[str]:
        return list(self.data["reportName"])

    @staticmethod
    def separate_benchmark_group_and_function(reportName: str) -> Tuple[str, str]:
        groupBenchmarkNames = reportName.split("/")
        return groupBenchmarkNames[0], groupBenchmarkNames[1]

    @staticmethod
    def get_function_name_from_experiment(experiment: str) -> str:
        _, name = BenchmarkData.separate_benchmark_group_and_function(experiment)
        return name

    def map_benchmark_groups_and_benchmark_indices(
        self,
    ) -> Dict[str, List[Tuple[int, str]]]:
        names: List[str] = list(self.get_report_names_in_order())
        groups: Dict[str, List[Tuple[int, str]]] = defaultdict(list)
        for i, name in enumerate(names):
            group: str
            name: str
            group, name = BenchmarkData.separate_benchmark_group_and_function(name)

            groups[group].append((i, name))

        return groups

    def get_benchmark_group_names(self) -> Set[str]:
        return set(self.map_benchmark_groups_and_benchmark_indices().keys())

    def get_benchmark_mean_from_index(self, index: int) -> float:
        return self.data["reportAnalysis"][index]["anMean"]["estPoint"]

    def get_benchmark_mean(self, group: str, function: str) -> float:
        index: int = self.get_benchmark_index(group, function)
        return self.get_benchmark_mean_from_index(index)

    def get_means_of_benchmark_list(self, indices: List[int]) -> List[float]:
        return list(map(self.get_benchmark_mean_from_index, indices))

    def get_tuple_count(self) -> int:
        return self.tuple_count

    @staticmethod
    def infer_tuple_count_from_path(path: str):
        file_name: str = path.split("/")[-1]
        if file_name.startswith("joinbench"):
            file_name_without_extension: str = file_name.split(".")[0]
            tuple_count_in_name: str = file_name_without_extension[len("joinbench") :]
            return BenchmarkData(path, int(tuple_count_in_name))
        raise Exception("Could not infer tuple count from path")

    def get_benchmark_index(self, group: str, function: str) -> int:
        map = self.map_benchmark_groups_and_benchmark_indices()
        indexed_function_list = map[group]
        for index, name in indexed_function_list:
            if name == function:
                return index

        raise Exception("Could not find benchmark index")

    def get_function_name_list(self) -> List[str]:
        return list(
            set(
                map(
                    lambda experiment: BenchmarkData.get_function_name_from_experiment(
                        experiment
                    ),
                    self.get_report_names_in_order(),
                )
            )
        )
