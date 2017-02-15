from setuptools import setup

setup(name='hello',
      version='0.0',
      description='hello',
      packages=['.'],
      entry_points="""
      [console_scripts]
hello=hello:main
"""
)
