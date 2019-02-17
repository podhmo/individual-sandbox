import sys
import os.path
import jinja2
import jinja2.ext as ext  # withを使うために利用


class Environment(jinja2.Environment):
    def join_path(self, name, parent, *, start=None):
        # xxx: FileSystemLoaderに依存した実装
        start = start or self.loader.searchpath[0]

        if parent is None:
            return name
        path = os.path.normpath(
            os.path.join(os.path.abspath(os.path.dirname(parent)), name)
        )
        return os.path.relpath(path, start=start)


env = Environment(loader=jinja2.FileSystemLoader(os.getcwd()), extensions=[ext.with_])
template = env.get_template(sys.argv[1])
print(template.render())
