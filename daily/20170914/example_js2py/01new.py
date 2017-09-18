def wrap(cls):
    def new(range_, loose):
        if isinstance(range_, cls):
            if range_.loose == loose:
                return range_
            else:
                return cls(range_.range, loose)
        return cls(range_, loose)

    return new


@wrap
class Range:
    def __init__(self, range_, loose):
        self.range = range_
        self.loose = loose


r = Range(">=1.2.0", True)
print(r.range)
r2 = Range(r, True)
print(r2.range)
r3 = Range(r, False)
print(r3.range)
print(isinstance(r3, Range))  # error
