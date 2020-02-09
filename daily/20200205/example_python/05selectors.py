import selectors

s = selectors.DefaultSelector()
io = open("/tmp/x.lock")
s.register(io,  selectors.)
