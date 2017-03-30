import itertools


class Rel(object):
    def __init__(self, value):
        self.value = value
        self.bigger = set()

    def __repr__(self):
        return "R[{} < {}]".format(self.value, self.bigger)


class Node(object):
    def __init__(self, value, forward):
        self.value = value
        self.forward = forward
        self.downside = None

    def __repr__(self):
        return "N[{}]".format(self.value)


class Detector(object):
    def __init__(self):
        self.rels = {}
        self.node = None

    def make_node(self, itr):
        try:
            x = next(itr)
            return Node(x, self.make_node(itr))
        except StopIteration:
            return None

    def add(self, xs):
        itr = iter(xs)
        if self.node is None:
            node = self.make_node(itr)
            self.node = node
            return
        self.move(self.node, next(itr), itr)

    def move(self, node, x, itr):
        if node.value == x:
            if node.forward is None:
                node.forward = self.make_node(itertools.chain([x], itr))
            else:
                self.move(node.forward, next(itr), itr)
        elif node.downside is None:
            if node.value not in self.rels:
                self.rels[node.value] = Rel(node.value)
            self.rels[node.value].bigger.add(x)
            node.downside = self.make_node(itertools.chain([x], itr))
        else:
            self.rels[node.value].bigger.add(x)
            self.move(node.downside, x, itr)
