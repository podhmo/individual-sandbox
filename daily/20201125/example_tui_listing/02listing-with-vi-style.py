from bullet import Check, keyhandler, styles
from bullet.charDef import NEWLINE_KEY, ARROW_DOWN_KEY, ARROW_UP_KEY


class MinMaxCheck(Check):
    def __init__(self, min_selections=0, max_selections=None, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.min_selections = min_selections
        self.max_selections = max_selections
        if max_selections is None:
            self.max_selections = len(self.choices)

    @keyhandler.register(NEWLINE_KEY)
    def accept(self):
        if self.valid():
            return super().accept()

    @keyhandler.register(ord("j"))
    @keyhandler.register(ARROW_DOWN_KEY)
    def moveDown(self):
        return super().moveDown()

    @keyhandler.register(ord("k"))
    @keyhandler.register(ARROW_UP_KEY)
    def moveUp(self):
        return super().moveUp()

    @keyhandler.register(ord("q"))
    def quit(self):
        self.interrupt()

    def valid(self):
        return (
            self.min_selections
            <= sum(1 for c in self.checked if c)
            <= self.max_selections
        )


client = MinMaxCheck(
    prompt="Choose 2 or 3 from the list: ",
    min_selections=2,
    max_selections=3,
    return_index=True,
    **styles.Example,
    **styles.Exam,
)
print("")
try:
    result = client.launch()
    print(result)
except KeyboardInterrupt:
    pass
