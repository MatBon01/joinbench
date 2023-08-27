from typing import List, Tuple

import numpy as np

from .benchmark_data import BenchmarkData


class EvaluationMethodPlotter:
    def __init__(self, nf_data: BenchmarkData, whnf_data: BenchmarkData):
        # assume nf and whfn have same tuple count
        self.nf_data = nf_data
        self.whnf_data = whnf_data

    def plot_benchmark_group_means_comparison(self, ax, group_name: str):
        whnf_map = self.whnf_data.map_benchmark_groups_and_benchmark_indices()

        indices: List[int]
        experiment_names: List[str]
        experiments: List[Tuple[int, str]] = whnf_map[group_name]
        indices, experiment_names = zip(*experiments)

        evaluation_method_means = {
            "nf": self.nf_data.get_means_of_benchmark_list(indices),
            "whnf": self.whnf_data.get_means_of_benchmark_list(indices),
        }

        bar_width = 0.25
        method_pos = 0

        experiment_positions = np.arange(len(experiments))
        for method, mean in evaluation_method_means.items():
            offset = bar_width * method_pos
            ax.bar(experiment_positions + offset, mean, bar_width, label=method)
            method_pos += 1

        ax.set_ylabel("Mean time (s)")
        ax.set_xlabel("Function")
        ax.set_title(
            f"Mean time to execute `{group_name}'\nquery by evaluation method with {self.nf_data.get_tuple_count()} tuples"
        )
        ax.set_xticks(experiment_positions + bar_width, experiment_names)
        ax.legend(title="Evaluation method", loc="upper right", ncols=3)

        return ax
