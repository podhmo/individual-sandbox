def hello(name, *, message: str = "hello world"):
    """greeting message"""
    print(f"{name}: {message}")
