from distutils.core import setup, Extension
from Cython.Build import cythonize

ext = Extension("prime", sources=["prime.pyx"])
setup(name="prime", ext_modules=cythonize([ext]))
