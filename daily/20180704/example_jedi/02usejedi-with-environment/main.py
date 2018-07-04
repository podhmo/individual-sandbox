import jedi
import os.path
from dictknife.loading import dumps

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--venv", default=None)
    args = parser.parse_args()
    environment = None

    if args.venv is not None:
        venv_path = os.path.expanduser(args.venv)
        environment = jedi.create_environment(venv_path)

    defs = jedi.Script(
        """import marshmallow""", 1, 7, ".", environment=environment
    ).goto_definitions()
    print(dumps({d.name: d.module_path for d in defs}, format="json"))
