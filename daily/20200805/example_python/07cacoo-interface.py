from __future__ import annotations
import typing as t
import typing_extensions as tx


class 車(tx.Protocol):
    def 走る(self):
        pass

    def 止まる(self):
        pass

    def 曲がる(self):
        pass


class 乗用車(車):
    def 音楽を鳴らす(self):
        pass

    def 充電する(self):
        pass


class バス(車):
    def アナウンスをする(self):
        pass

    def ドアを開く(self):
        pass


class トラック(車):
    def 荷物を積む操作(self):
        pass

    def 荷物を下ろす操作(self):
        pass
