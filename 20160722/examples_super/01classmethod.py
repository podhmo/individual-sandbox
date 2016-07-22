class A:
    @classmethod
    def for_class(cls):
        print("A", cls)

    @classmethod
    def for_class_oldstyle(cls):
        print("A", cls)


class B(A):
    @classmethod
    def for_class(cls):
        super().for_class()
        print("B", cls)

    @classmethod
    def for_class_oldstyle(cls):
        super(B, cls).for_class_oldstyle()
        print("B", cls)


B.for_class()
B.for_class_oldstyle()
