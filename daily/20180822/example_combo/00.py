class Select:
    def __init__(self, name, candidates, value=None):
        self.name = name
        self.candidates = candidates
        self.value = value
        if self.value is None and len(self.candidates) > 0:
            self.value = self.candidates[0]

    def __str__(self):
        return f"{self.name}: {self.value}"

    def select(self, value):
        for v in self.candidates:
            if v == value:
                self.value = value


s0 = Select("TOP", ["A", "B", "C", "D", "E"])
print(s0)
s0.select("C")
print(s0)
