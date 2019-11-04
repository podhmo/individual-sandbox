# from: https://docs.ponyorm.org/relationships.html
from __future__ import annotations
import typing as t


class Customer:
    name: str
    orders: t.List[Order]


class Order:
    orderd_at: str  # datetime?
    customer: Customer
    items: t.List[OrderItem]


class OrderItem:
    order: Order
    n: int
    product: Product


class Product:
    name: str
    tags: t.List[Tag]


class Tag:
    name: str
    products: t.List[Product]
