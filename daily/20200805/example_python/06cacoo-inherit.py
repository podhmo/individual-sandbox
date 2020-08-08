from __future__ import annotations
import typing as t


class 社員:
    名前: str
    入社日: str

    def 勤怠入力をする(self):
        ...


class 技術社員(社員):
    def システム開発を行う(self):
        ...


class 営業社員(社員):
    def 営業を行う(self):
        ...


class 事務社員(社員):
    def 事務処理を行う(self):
        ...
