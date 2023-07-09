from test_utils.benchmark_utils import get_test_benchmark_data

from joinbench.evaluation_method_plotter import EvaluationMethodPlotter


class TestEvaluationMethodPlotter:
    def test_constructor_takes_in_two_evaluation_method_data_classes(self):
        nf = get_test_benchmark_data()
        whnf = get_test_benchmark_data()
        assert EvaluationMethodPlotter(nf, whnf)
