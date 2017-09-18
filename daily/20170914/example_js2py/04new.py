class RangeMeta(type):
    def __subclasshook__(cls, range_, loose):
        if isinstance(range_, cls):
            if range_.loose == loose:
                return range_
            else:
                return cls(range_.range, loose)
        return super().__call__(range_, loose)


class Range(metaclass=RangeMeta):
    def __init__(self, range_, loose):
        self.range = range_
        self.loose = loose


r = Range(">=1.2.0", True)
print(r.range)
r2 = Range(r, True)
print(r2.range)
r3 = Range(r, False)
print(r3.range)
print(isinstance(r3, Range))
