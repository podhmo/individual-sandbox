class A:
    def __new__(cls, instance=None, **kwargs):
        context = kwargs.get("context")
        if context:
            context["hmm"] = "hmm"
        return super().__new__(cls)

    def __init__(self, instance=None, **kwargs):
        self.instance = instance
        self.kwargs = kwargs

print(A(object(), context={"are": "hai"}).kwargs)
