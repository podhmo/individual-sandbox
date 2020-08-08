from __future__ import annotations
import typing as t
import typing_extensions as tx

# https://cacoo.com/ja/blog/entity-relationship-diagram-for-beginner/


class _Key:
    def __init__(self, key: str) -> None:
        self.key = key


class Student:
    student_id: int
    student_name: str
    student_address: int

    seat: tx.Annotated[Seat, _Key("Seat.seat_number")]


class Seat:
    seat_number: int
    seat_position: str  # E6,E7...


class Cource:
    cource_name: str
    student_number: int


class Instructor:
    instructor_number: int
    instructor_name: str
    instructor_faculty: str


class Class:
    cource_name: str
    section_number: int
    num_registerd: str
    class_date_time: str


class Section:
    section_number: int


class Professor:
    professor_id: int
    professor_name: str
    professor_faculty: str
