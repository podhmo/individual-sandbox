from functools import partial
from responder import API
from responder.background import BackgroundQueue
import responder.api

# patch (XXX: black magic)
responder.api.BackgroundQueue = partial(BackgroundQueue, 1000)
api = API()

print(api.background)
print(vars(api.background))
