from bullet import Bullet

cli = Bullet(
    prompt="\nPlease choose a fruit: ",
    choices=["apple", "banana", "orange", "watermelon", "strawberry"],
    return_index=True,
)

result = cli.launch()
print("You chose:", result)
