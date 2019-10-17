import runpy

runpy.run_path("hello.py", run_name="hello", init_globals={"NAME": "foo"})
