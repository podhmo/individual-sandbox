import sys
from setuptools import setup, find_packages
from setuptools.command.test import test as TestCommand

install_requires = [
    'django',
]


class MyTest(TestCommand):
    # if you want to run specific tests only.
    # `python setup.py test -s  django_foo.tests.<testfile>.<class>.<method>`

    user_options = TestCommand.user_options + [
        ('logging=', 'l', "logging"),
    ]

    def initialize_options(self):
        self.logging = None
        super(MyTest, self).initialize_options()

    def finalize_options(self):
        super(MyTest, self).finalize_options()
        if self.logging is not None:
            import logging
            logging.basicConfig(level=getattr(logging, self.logging.upper(), 0))

    def run_tests(self):
        import os
        if "DJANGO_SETTINGS_MODULE" not in os.environ:
            os.environ["DJANGO_SETTINGS_MODULE"] = "django_foo.tests.settings"
        from django.test.utils import get_runner
        from django.conf import settings
        import django
        django.setup()
        sys.exit(get_runner(settings)().run_tests([self.test_args[-1]]))


setup(name='django-foo',
      packages=find_packages(exclude=["django_foo.tests"]),
      install_requires=install_requires,
      test_suite="django_foo.tests",
      cmdclass={"test": MyTest})
