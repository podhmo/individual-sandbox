import inquirer
import string
from inquirer import themes
from inquirer.render.console import ConsoleRender, List
from readchar import key


class ExtendedConsoleRender(ConsoleRender):
    def render_factory(self, question_type):
        if question_type == "list":
            return ExtendedList
        return super().render_factory(question_type)


CTRL_MAP = {c: chr(i) for i, c in enumerate(string.ascii_uppercase, 1)}


class ExtendedList(List):
    def process_input(self, pressed):
        # emacs style
        if pressed in (CTRL_MAP["B"], CTRL_MAP["P"]):
            pressed = key.UP
        elif pressed in (CTRL_MAP["F"], CTRL_MAP["N"]):
            pressed = key.DOWN
        elif pressed == CTRL_MAP["G"]:
            pressed = CTRL_MAP["C"]
        elif pressed == CTRL_MAP["A"]:
            self.current = 0
            return
        elif pressed == CTRL_MAP["G"]:
            self.current = len(self.question.choices) - 1
            return

        # vi style
        if pressed in ("k", "h"):
            pressed = key.UP
        elif pressed in ("j", "l"):
            pressed = key.DOWN
        elif pressed == "q":
            pressed = key.CTRL_C

        # effect (rendering)
        super().process_input(pressed)


questions = [
    inquirer.List(
        "size",
        message="What size do you need?",
        choices=["Jumbo", "Large", "Standard", "Medium", "Small", "Micro"],
        carousel=True,
    )
]

answers = inquirer.prompt(questions, render=ExtendedConsoleRender(theme=themes.GreenPassion()))
print(answers)
