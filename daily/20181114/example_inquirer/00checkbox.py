from pprint import pprint
import inquirer

questions = [
    inquirer.Checkbox(
        'interests',
        message="What are you interested in?",
        choices=['Computers', 'Books', 'Science', 'Nature', 'Fantasy', 'History'],
        default=['Computers', 'Books']
    ),
]

answers = inquirer.prompt(questions)
pprint(answers)
