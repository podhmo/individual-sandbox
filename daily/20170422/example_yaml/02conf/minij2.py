# minimum js2cli. (becuase j2cli doesn't suport python 3.x)
import sys
import jinja2
import json


def main(template_path, config):
    env = jinja2.Environment(loader=jinja2.FileSystemLoader(["."]), undefined=jinja2.StrictUndefined)
    t = env.get_or_select_template(template_path)
    data = json.loads(t.render(**config))
    json.dump(data, sys.stdout, indent=2, ensure_ascii=False, sort_keys=True)


if __name__ == "__main__":
    template, config = sys.argv[1:3]
    with open(config) as rf:
        config = json.load(rf)
    main(template, config)
