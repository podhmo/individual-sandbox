from goaway.commands.swagger2go import Walker


class MyWalker(Walker):
    def resolve_tag(self, name):
        # return ' `json:"{name}"`'.format(name=name)
        return ' `json:"{name}" bson:"{name}"`'.format(name=name)
