import asyncio

from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.ext.asyncio import create_async_engine
from sqlalchemy.future import select
from sqlalchemy.orm import registry
from sqlalchemy.orm import sessionmaker


class User:
    __table_args__ = {"autoload": True}
    id = Column(Integer, primary_key=True)


async def async_main():
    engine = create_async_engine(
        "sqlite+aiosqlite:///data.db",
        echo=True,
    )
    mapper_registry = registry()
    metadata = MetaData()
    metadata.bind = engine
    async with engine.connect() as conn:
        await conn.run_sync(metadata.reflect)

    mapper_registry.map_imperatively(User, metadata.tables["User"])
    async_session = sessionmaker(engine, expire_on_commit=False, class_=AsyncSession)

    async with async_session() as session:
        result = await session.execute(select(User).order_by(User.id))
        for x in result.scalars():
            print(x, x.id, x.name)  # nameがautoloadで生えた


asyncio.run(async_main())
