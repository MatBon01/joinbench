import json
from typing import List, Set, Tuple


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
    def _separate_group_from_name(reportName: str) -> Tuple[str, str]:
        groupBenchmarkNames = reportName.split("/")
        return groupBenchmarkNames[0], groupBenchmarkNames[1]
