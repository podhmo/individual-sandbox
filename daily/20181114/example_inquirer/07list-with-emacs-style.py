import inquirer
from inquirer import themes
from inquirer.render.console import ConsoleRender, List
from readchar import key


class ExtendedConsoleRender(ConsoleRender):
    def render_factory(self, question_type):
        if question_type == "list":
            return ExtendedList
        return super().render_factory(question_type)


# [(hex(i), c) for i, c in enumerate(string.ascii_lowercase, 1)]
CTRL_G = "\x07"
CTRL_N = "\x0e"
CTRL_P = "\x10"


class ExtendedList(List):
    def process_input(self, pressed):
        # emacs style
        if pressed in (key.CTRL_B, CTRL_P):
            pressed = key.UP
        elif pressed in (key.CTRL_F, CTRL_N):
            pressed = key.DOWN
        elif pressed == CTRL_G:
            pressed = key.CTRL_C
        elif pressed == key.CTRL_A:
            self.current = 0
            return
        elif pressed == key.CTRL_E:
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
