from zenmai.core import Evaluator as _Evaluator
from zenmai.driver import Driver as _Driver


class Evaluator(_Evaluator):
    def eval(self, context, d):
        if hasattr(d, "keys"):
            return self.eval_dict(context, d)
        elif isinstance(d, (list, tuple)):
            return self.eval_list(context, d)
        elif str(d).startswith("@"):
            return self.eval(context, {"$get": d[1:]})
        else:
            return d


class Driver(_Driver):
    evaluator_factory = Evaluator
