# -*- coding:utf-8 -*-
from setuptools import setup, find_packages
from setuptools.command.test import test


class Test(test):
    user_options = test.user_options + [
        ("ignore=", None, "ignore marked"),
        ("only=", None, "only marked"),
    ]

    def initialize_options(self):
        self.ignore = None
        self.only = None
        super().initialize_options()

    def run(self):
        from marking import markers
        if self.only is not None:
            self.only = [x.strip() for x in self.only.split(",")]
            for name in self.only:
                markers.register(name, "skip_deactivate")
            markers.register("", "skip_activate")
        elif self.ignore is not None:
            self.ignore = [x.strip() for x in self.ignore.split(",")]
            for name in self.ignore:
                markers.register(name, "skip_activate")
        return super().run()


setup(
    name='marking',
    version='0.0',
    description='-',
    packages=find_packages(exclude=["marking.tests"]),
    test_suite="marking.tests",
    cmdclass={"test": Test}
)
