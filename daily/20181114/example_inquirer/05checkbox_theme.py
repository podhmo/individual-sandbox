from pprint import pprint
import inquirer
from inquirer.themes import GreenPassion

questions = [
    inquirer.Checkbox(
        'interests',
        message="What are you interested in?",
        choices=['Computers', 'Books', 'Science', 'Nature', 'Fantasy', 'History'],
        default=['Computers', 'Books']
    ),
]

answers = inquirer.prompt(questions, theme=GreenPassion())
pprint(answers)
