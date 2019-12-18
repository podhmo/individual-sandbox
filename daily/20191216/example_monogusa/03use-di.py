from monogusa import component


def hello(database_url: str) -> None:
    print(f"db from {database_url}")


@component
def database_url() -> str:
    return "sqlite:///:memory:"
