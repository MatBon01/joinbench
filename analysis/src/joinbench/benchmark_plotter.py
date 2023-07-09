from typing import Dict, List, Tuple

from .benchmark_data import BenchmarkData


class BenchmarkPlotter:
    def __init__(self, data: BenchmarkData):
        self.data = data

    def plot_benchmark_group_means(self, ax, group_name: str):
        group_map: Dict[
            str, List[Tuple[int, str]]
        ] = self.data.map_benchmark_groups_and_benchmark_indices()
        experiments: List[Tuple[int, str]] = group_map[group_name]

        indices: List[int]
        names: List[str]
        indices, names = zip(*experiments)
        means = self.data.get_means_of_benchmark_list(indices)

        ax.set_title(f"Mean time to complete '{group_name}'")
        ax.set_ylabel("Time (s)")
        ax.set_xlabel("Function")

        ax.bar(names, means)

        return ax
