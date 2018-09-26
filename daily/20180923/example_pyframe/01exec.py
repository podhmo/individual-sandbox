import logging

code = """
import inspect

def MAIN():
    f = inspect.currentframe()
    co = f.f_code
    print(co.co_filename, f.f_lineno, co.co_name)
""".strip()

exec(code, logging.__dict__)

logging.MAIN()
