from zenmai.compile import Evaluator


def eval(self, context, d):
    if hasattr(d, "keys"):
        return self.eval_dict(context, d)
    elif isinstance(d, (list, tuple)):
        return self.eval_list(context, d)
    elif str(d).startswith("@"):
        return self.eval(context, {"$get": d[1:]})
    else:
        return d


Evaluator.eval = eval
