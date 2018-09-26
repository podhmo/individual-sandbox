import logging


class Mock:
    def __init__(self):
        self.names = []

    def __getitem__(self, name):
        self.names.append(name)
        return name


m = Mock()
print(logging.BASIC_FORMAT % m)
print(m.names)
