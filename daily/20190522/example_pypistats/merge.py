from magicalimport import import_from_physical_path

try:
    m = import_from_physical_path("../../20190521/example_dict/merge.p")
except AttributeError:
    m = import_from_physical_path("../../20190521/example_dict/merge.py")


def merge(left, right, *, prefix="r_"):
    merge_k = "category"
    right = m.with_prefix(prefix, right, exclude=[merge_k])
    return m.merge(left, right, how="outer", on=merge_k)
