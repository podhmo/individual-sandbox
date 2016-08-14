# -*- coding:utf-8 -*-
import json
import shlex
import copy
import re
from collections import OrderedDict


class Control(object):
    def __init__(self, root):
        self.stack = [root]
        self.root = root
        self.path = []

    def push(self, name):
        self.stack.append(self.stack[-1][name])
        self.path.append(name)

    def pop(self):
        self.stack.pop()
        self.path.pop()

    def refresh(self):
        self.stack = [self.root]
        self.path = []

    def nodes_from_path(self, path):
        return [(int(x) if x.isdigit() else x) for x in path.strip().split("/")]

    def access(self, nodes):
        if not nodes:
            return self.stack[-1]

        stack = self.stack[:]
        if nodes[0] == "":
            stack = [self.root]

        for name in non_empty_iterator(nodes):
            if name == ".":
                continue
            elif name == "..":
                stack.pop()
            else:
                stack.append(stack[-1][name])
        return stack[-1]

    @property
    def current(self):
        return self.stack[-1]


def non_empty_iterator(itertaor):
    for x in itertaor:
        if not x:
            continue
        yield x


class SelfishList(list):
    def __setitem__(self, i, v):
        try:
            super(SelfishList, self).__setitem__(i, v)
        except IndexError:
            if len(self) == i:
                self.append(v)
            else:
                raise


class JSONMaker(object):
    def __init__(self):
        self.list = SelfishList
        self.dict = OrderedDict

    def mkobject(self, c, *paths):
        for path in non_empty_iterator(paths):
            nodes = c.nodes_from_path(path)
            target = c.access(nodes[:-1])
            if nodes[-1] not in target:
                target[nodes[-1]] = self.dict()

    def mkarray(self, c, *paths):
        for path in non_empty_iterator(paths):
            nodes = c.nodes_from_path(path)
            target = c.access(nodes[:-1])
            if nodes[-1] not in target:
                target[nodes[-1]] = self.list()

    def cp(self, c, src, dst):
        srcobj = c.access(c.nodes_from_path(src))
        nodes = c.nodes_from_path(dst)
        target = c.access(nodes[:-1])
        target[nodes[-1]] = copy.deepcopy(srcobj)

    def rm(self, c, *paths):
        for path in non_empty_iterator(paths):
            nodes = c.nodes_from_path(path)
            target = c.access(nodes[:-1])
            target.pop(nodes[-1], None)

    def put(self, c, path, v):
        nodes = c.nodes_from_path(path)
        target = c.access(nodes[:-1])
        target[nodes[-1]] = v

    def append(self, c, path, v):
        nodes = c.nodes_from_path(path)
        target = c.access(nodes)
        target.append(v)

    def cd(self, c, v):
        if not v:
            return
        elif v.startswith("/"):
            self.refresh()
            self.cd(c, v[1:])
        elif v.startswith("./"):
            self.cd(c, v[2:])
        else:
            for name in non_empty_iterator(c.nodes_from_path(v)):
                if name == "..":
                    c.pop()
                else:
                    c.push(name)

    def eval(self, cmd,
             splitter=re.compile("[;\n]", re.MULTILINE).split):
        c = Control(self.dict())
        actions = splitter(cmd)
        for action in actions:
            if not action:
                continue
            name, *args = shlex.split(action)
            getattr(self, name)(c, *args)
        return c.root

    def pwd(self, c):
        print(c.path)


v = JSONMaker().eval("""
mkobject address phoneNumber
cd address
put ./streetAddress "21 2nd Street"
put city "New York"
cd ../phoneNumber
put location home
put code 44
mkarray ../favorites
append ../favorites a
append /favorites b
""")
print(json.dumps(v, indent=2))
"""
{
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York"
  },
  "phoneNumber": {
    "code": "44",
    "location": "home"
  },
  "favorites": [
    "a",
    "b"
  ]
}
"""

print(json.dumps(JSONMaker().eval("""
mkobject foo
cd foo; put name "foo"; put age 20
cd ../
mkarray members
cp foo members/0
cp foo members/1
put members/1/name "bar"
rm foo
"""), indent=2))


print(json.dumps(JSONMaker().eval("""
mkobject address phoneNumber
cd address
put ./streetAddress "21 2nd Street"
put city "New York"
put ../phoneNumber/location home
put /phoneNumber/code 44
"""), indent=2))
