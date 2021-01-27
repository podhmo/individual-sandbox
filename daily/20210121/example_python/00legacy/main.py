import abc


class Node(abc.ABC):
    @abc.abstractproperty
    def type_(self):
        pass

    def __init__(self, value):
        self.value = value


class ListNode(abc.ABC):
    @abc.abstractproperty
    def type_(self):
        pass

    def __init__(self, children):
        self.children = children

    def add(self, node):
        self.children.append(node)


class H1(Node):
    type_ = "h1"


class H2(Node):
    type_ = "h2"


class H3(Node):
    type_ = "h3"


class H4(Node):
    type_ = "h4"


class Text(Node):
    type_ = "text"


class Article(ListNode):
    type_ = "article"


class ItemList(ListNode):
    type_ = "itemize"


class NumberdItemList(ListNode):
    type_ = "enumerate"


class Emitter(abc.ABC):
    def emit(self, node):
        if hasattr(node, "children"):
            yield from self.emit_list_node(node)
        else:
            yield from self.emit_value_node(node)

    @abc.abstractmethod
    def emit_list_node(self, node):
        raise NotImplementedError("")

    @abc.abstractmethod
    def emit_value_node(self, node):
        raise NotImplementedError("")


class MarkdownEmitter(Emitter):
    def emit_list_node(self, node):
        typ = node.type_
        if typ == "article":
            return ("\n".join(self.emit(child)) for child in node.children)
        elif typ == "itemize":
            return (f"- {''.join(self.emit(child))}" for child in node.children)
        elif typ == "enumerate":
            return (
                f"{i}. {''.join(self.emit(child))}"
                for i, child in enumerate(node.children, 1)
            )
        else:
            raise ValueError(f"unexpected list node type: {typ}")

    def emit_value_node(self, node):
        if not hasattr(node, "type_"):  # str
            yield node
            return

        typ = node.type_
        if typ == "h1":
            yield f"# {node.value}"
        elif typ == "h2":
            yield f"## {node.value}"
        elif typ == "h3":
            yield f"### {node.value}"
        elif typ == "h4":
            yield f"#### {node.value}"
        elif typ == "text":
            yield node.value
        else:
            raise ValueError(f"unexpected value node type: {typ}")


class HTMLEmitter(Emitter):
    def emit_list_node(self, node):
        typ = node.type_
        if typ == "article":
            yield "<article>"
            for child in node.children:
                yield "\n".join(self.emit(child))
            yield "</article>"
        elif typ == "itemize":
            yield "<ul>"
            for child in node.children:
                yield f"<li>{''.join(self.emit(child))}</li>"
            yield "</ul>"
        elif typ == "enumerate":
            yield "<ol>"
            for child in node.children:
                yield f"<li>{''.join(self.emit(child))}</li>"
            yield "</ol>"
        else:
            raise ValueError(f"unexpected list node type: {typ}")

    def emit_value_node(self, node):
        if not hasattr(node, "type_"):  # str
            yield node
            return

        typ = node.type_
        if typ == "h1":
            yield f"<h1>{node.value}</h1>"
        elif typ == "h2":
            yield f"<h2>{node.value}</h2>"
        elif typ == "h3":
            yield f"<h3>{node.value}</h3>"
        elif typ == "h4":
            yield f"<h4>{node.value}</h4>"
        elif typ == "text":
            yield f"<p>{node.value}</p>"
        else:
            raise ValueError(f"unexpected value node type: {typ}")


def emit(node, *, emitter=None):
    emitter = emitter or MarkdownEmitter()
    return "\n".join(list(emitter.emit(node)))


# print(emit(Text("hello")))
print(
    emit(
        Article(
            [
                H1("hello"),
                Text("This is my first article."),
                ItemList(["foo", "bar", "boo"]),
                NumberdItemList(["foo", "bar", "boo"]),
                ItemList(["xxx", NumberdItemList(["a", "b", "c"]), "zzz"]),
                Text("fin."),
            ]
        )
    )
)
