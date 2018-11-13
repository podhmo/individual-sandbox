import inquirer

questions = [
    inquirer.List(
        "size",
        message="What size do you need?",
        choices=[(f"Choice {i}", i) for i in range(100)],
        carousel=True,
    )
]

# hmm,
print(inquirer.prompt(questions))
