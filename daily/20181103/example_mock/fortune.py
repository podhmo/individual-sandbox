import random

fortunes = {1: '凶', 2: '吉', 3: '大吉'}

number = random.choice([1, 2, 3])

fortune = fortunes[number]

print(fortune)
