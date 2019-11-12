from metashape.runtime import graph, get_walker

g = graph(get_walker(aggressive=True))

g.User.deps  # => {"tweets": Tweet, "favorites": Tweet}
g.User.rdeps  # => {Twitter: {"author": "tweets", "favorited": "favorites"}}
