# sample.py - mini-python-ts-mode テスト用コード

import os
import sys
from typing import Optional

# M-x mini-python-ts-mode

# 定数
MAX_RETRIES = 3
PI = 3.14159
DEBUG = Falsey


class Animal:
    """動物の基底クラス"""

    def __init__(self, name: str, age: int = 0):
        self.name = name
        self.age = age

    def speak(self) -> str:
        raise NotImplementedError

    def __repr__(self) -> str:
        return f"Animal(name={self.name!r}, age={self.age})"


class Dog(Animal):
    """犬クラス"""

    def speak(self) -> str:
        return f"{self.name} says: Woof!"

    def fetch(self, item: str = "ball") -> None:
        print(f"{self.name} fetched the {item}!")


def greet(name: str, loud: bool = False) -> str:
    """挨拶文を返す"""
    message = f"Hello, {name}!"
    if loud:
        return message.upper()
    return message


def factorial(n: int) -> int:
    """再帰で階乗を計算する"""
    if n <= 1:
        return 1
    return n * factorial(n - 1)


def find_evens(numbers: list) -> list:
    """偶数のみを返す"""
    return [x for x in numbers if x % 2 == 0]


# デコレータの例
def retry(times: int = MAX_RETRIES):
    def decorator(func):
        def wrapper(*args, **kwargs):
            for attempt in range(times):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == times - 1:
                        raise
                    print(f"Retry {attempt + 1}/{times}: {e}")
        return wrapper
    return decorator


@retry(times=2)
def unstable_operation(x: int) -> int:
    """不安定な処理（テスト用）"""
    if x < 0:
        raise ValueError(f"Negative value: {x}")
    return x * 2


# ジェネレータ
def countdown(n: int):
    """カウントダウンジェネレータ"""
    while n > 0:
        yield n
        n -= 1


# 型チェックの例
def process(value: Optional[int]) -> str:
    match value:
        case None:
            return "nothing"
        case 0:
            return "zero"
        case x if x > 0:
            return f"positive: {x}"
        case _:
            return f"negative: {value}"


if __name__ == "__main__":
    # 基本的な使用例
    dog = Dog("Rex", age=3)
    print(dog.speak())
    dog.fetch("stick")

    print(greet("World"))
    print(greet("World", loud=True))

    print(factorial(5))           # 120
    print(find_evens(range(10)))  # [0, 2, 4, 6, 8]

    for n in countdown(3):
        print(n)

    for v in [None, 0, 42, -1]:
        print(process(v))

    # 真偽値・None
    flags = [True, False, None]
    print(all(x is not None for x in flags))

    # 文字列操作
    words = ["hello", "world"]
    joined = ", ".join(words)
    print(joined.upper())

    # 数値リテラル
    hex_val = 0xFF
    bin_val = 0b1010
    float_val = 1.5e3
    print(hex_val, bin_val, float_val)
