from __future__ import annotations
import typing as t
import typing_extensions as tx


# http://agile.blog.jp/agile_scrum/14997606.html
# https://cdn.visual-paradigm.com/guide/uml/uml-class-diagram-tutorial/18-uml-class-diagram-example-gui.png


class Frame:
    pass


class Window(Frame):
    shapes: t.List[Shape]
    context: DrawingContext


class ConsoleWindow(Window):
    pass


class DialogBox(Window):
    controller: DataController


class Event:
    pass


class Shape:
    pass


class Circle(Shape):
    point: Point


class Point:
    pass


class Rectanble(Shape):
    pass


class Polygon(Shape):
    pass


class DataController:
    pass


class DrawingContext:
    pass
