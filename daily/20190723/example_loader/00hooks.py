import sys

# see: https://docs.python.org/ja/3/reference/import.html#finders-and-loaders

__loader__  # => <class '_frozen_importlib.BuiltinImporter'>

sys.meta_path  # => [<class '_frozen_importlib.BuiltinImporter'>, <class '_frozen_importlib.FrozenImporter'>, <class '_frozen_importlib_external.PathFinder'>, <pkg_resources.extern.VendorImporter object at 0x7f1b480307b8>, <pkg_resources._vendor.six._SixMetaPathImporter object at 0x7f1b4806e7b8>]

sys.path_hooks  # => [<class 'zipimport.zipimporter'>, <function FileFinder.path_hook.<locals>.path_hook_for_FileFinder at 0x7f1b48d2a598>]

