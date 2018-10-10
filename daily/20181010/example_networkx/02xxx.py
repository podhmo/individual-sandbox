# %matplotlib inline
from networkx import nx
"""setup"""

G = nx.lollipop_graph(4, 6)

pathlengths = []

for v in G.nodes():
    spl = dict(nx.single_source_shortest_path_length(G, v))
    for p in spl:
        pathlengths.append(spl[p])
"""draw"""

nx.draw(G, with_labels=True)
"""fancy draw"""

nx.draw(
    G,
    with_labels=True,
    node_size=1500,
    node_color="skyblue",
    node_shape="s",
    alpha=0.5,
    linewidths=40
)
