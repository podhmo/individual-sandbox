class Animal:
    def __init__(self, name: str) -> None:
        self.name = name


class Dog(Animal):
    def say(self):
        print("わんわん")


class Cat(Animal):
    def say(self):
        print("にゃーん")
