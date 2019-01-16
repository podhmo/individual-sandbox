import sys
import jedi
import textwrap


def run(module, venv_path=None):
    source = textwrap.dedent(f"""
    import {module}
    {module}.no
    """).strip()

    env = None
    if venv_path is not None:
        env = jedi.create_environment(venv_path)
    script = jedi.Script(source, 2, len(module + ".no") - 1, environment=env)
    print(script.completions())


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("python program.py <module name> [<venv-path>]", file=sys.stderr)
        sys.exit(1)
    run(*sys.argv[1:])
