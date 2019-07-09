__unittest = True  # <- this!!


class AlwaysFailMixin:
    def assert_always_fail(self):
        self.fail("fail..")
