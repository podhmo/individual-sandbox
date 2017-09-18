class Range:
    def __new__(cls, range_, loose):
        if isinstance(range_, Range):
            if range_.loose == loose:
                return range_
            else:
                return Range(range_.range, loose)
        return super().__new__(cls)

    def __init__(self, range_, loose):
        self.range = range_
        self.loose = loose


r = Range(">=1.2.0", True)
print(r.range)
r2 = Range(r, True)
print(r2.range)
r3 = Range(r, False)
print(r3.range)

