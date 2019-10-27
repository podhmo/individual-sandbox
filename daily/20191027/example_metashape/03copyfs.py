import fs
from magicalimport import import_symbol

# pip install fs magicalimport

create_fs = import_symbol("./02inmemory.py:create")


with create_fs() as my_fs:

    # copy filesystem
    from fs.copy import copy_fs  # noqa

    def on_copy(src_fs, src_name: str, dst_fs, dst_name: str) -> None:
        import sys

        print(f"copied {src_fs}:{src_name} -> {dst_name}", file=sys.stderr)

    copy_fs(my_fs, fs.open_fs("./"), on_copy=on_copy)
