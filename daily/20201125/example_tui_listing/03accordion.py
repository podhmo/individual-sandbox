import sys
from bullet import Check, keyhandler, styles
from bullet.charDef import NEWLINE_KEY, ARROW_DOWN_KEY, ARROW_UP_KEY, SPACE_CHAR


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

    @keyhandler.register(SPACE_CHAR)
    @keyhandler.register(ord("l"))
    def toggleRow(self):
        self.checked[self.pos] = not self.checked[self.pos]
        self.printRow(self.pos)

    def printRow(self, idx):
        from bullet import utils

        utils.forceWrite(" " * (self.indent + self.align))
        back_color = (
            self.background_on_switch if idx == self.pos else self.background_color
        )
        word_color = self.word_on_switch if idx == self.pos else self.word_color
        check_color = self.check_on_switch if idx == self.pos else self.check_color
        if self.checked[idx]:
            utils.cprint(
                "{}".format(self.check) + " " * self.margin,
                check_color,
                back_color,
                end="",
            )
        else:
            utils.cprint(
                " " * (len(self.check) + self.margin), check_color, back_color, end=""
            )
        utils.cprint(self.choices[idx], word_color, back_color, end="")
        utils.cprint(
            " " * (self.max_width - len(self.choices[idx])), on=back_color, end=""
        )
        utils.moveCursorHead()

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
