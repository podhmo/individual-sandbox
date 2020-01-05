from __future__ import annotations


def hello(db: DB) -> None:
    db.save("hello")


class DB:
    def save(self, msg: str) -> None:
        print(msg)


if __name__ == "__main__":
    from monogusa import component
    from monogusa.cli import run

    @component
    def db() -> DB:
        return DB()

    run()
