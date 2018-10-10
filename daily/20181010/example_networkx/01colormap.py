#    Copyright (C) 2004-2018 by
#    Aric Hagberg <hagberg@lanl.gov>
#    Dan Schult <dschult@colgate.edu>
#    Pieter Swart <swart@lanl.gov>
#    All rights reserved.
#    BSD license.

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
from networkx import nx

G = nx.lollipop_graph(4, 6)

pathlengths = []

print("source vertex {target:length, }")
for v in G.nodes():
    spl = dict(nx.single_source_shortest_path_length(G, v))
    print('{} {} '.format(v, spl))
    for p in spl:
        pathlengths.append(spl[p])

print('')
print("average shortest path length %s" % (sum(pathlengths) / len(pathlengths)))

# histogram of path lengths
dist = {}
for p in pathlengths:
    if p in dist:
        dist[p] += 1
    else:
        dist[p] = 1

print('')
print("length #paths")
verts = dist.keys()
for d in sorted(verts):
    print('%s %d' % (d, dist[d]))

print("radius: %d" % nx.radius(G))
print("diameter: %d" % nx.diameter(G))
print("eccentricity: %s" % nx.eccentricity(G))
print("center: %s" % nx.center(G))
print("periphery: %s" % nx.periphery(G))
print("density: %s" % nx.density(G))

nx.draw(
    G,
    with_labels=True,
    cmap=cm.Blues,
    node_color=np.arange(len(G)) / len(G),
    node_size=400,
    alpha=0.7,
)
plt.show()
"""Draw the graph G using Matplotlib.

Draw the graph with Matplotlib with options for node positions,
labeling, titles, and many other drawing features.
See draw() for simple drawing without labels or axes.

Parameters
----------
G : graph
   A networkx graph

pos : dictionary, optional
   A dictionary with nodes as keys and positions as values.
   If not specified a spring layout positioning will be computed.
   See :py:mod:`networkx.drawing.layout` for functions that
   compute node positions.

arrows : bool, optional (default=True)
   For directed graphs, if True draw arrowheads.
   Note: Arrows will be the same color as edges.

arrowstyle : str, optional (default='-|>')
    For directed graphs, choose the style of the arrowsheads.
    See :py:class: `matplotlib.patches.ArrowStyle` for more
    options.

arrowsize : int, optional (default=10)
   For directed graphs, choose the size of the arrow head head's length and
   width. See :py:class: `matplotlib.patches.FancyArrowPatch` for attribute
   `mutation_scale` for more info.

with_labels :  bool, optional (default=True)
   Set to True to draw labels on the nodes.

ax : Matplotlib Axes object, optional
   Draw the graph in the specified Matplotlib axes.

nodelist : list, optional (default G.nodes())
   Draw only specified nodes

edgelist : list, optional (default=G.edges())
   Draw only specified edges

node_size : scalar or array, optional (default=300)
   Size of nodes.  If an array is specified it must be the
   same length as nodelist.

node_color : color string, or array of floats, (default='r')
   Node color. Can be a single color format string,
   or a  sequence of colors with the same length as nodelist.
   If numeric values are specified they will be mapped to
   colors using the cmap and vmin,vmax parameters.  See
   matplotlib.scatter for more details.

node_shape :  string, optional (default='o')
   The shape of the node.  Specification is as matplotlib.scatter
   marker, one of 'so^>v<dph8'.

alpha : float, optional (default=1.0)
   The node and edge transparency

cmap : Matplotlib colormap, optional (default=None)
   Colormap for mapping intensities of nodes

vmin,vmax : float, optional (default=None)
   Minimum and maximum for node colormap scaling

linewidths : [None | scalar | sequence]
   Line width of symbol border (default =1.0)

width : float, optional (default=1.0)
   Line width of edges

edge_color : color string, or array of floats (default='r')
   Edge color. Can be a single color format string,
   or a sequence of colors with the same length as edgelist.
   If numeric values are specified they will be mapped to
   colors using the edge_cmap and edge_vmin,edge_vmax parameters.

edge_cmap : Matplotlib colormap, optional (default=None)
   Colormap for mapping intensities of edges

edge_vmin,edge_vmax : floats, optional (default=None)
   Minimum and maximum for edge colormap scaling

style : string, optional (default='solid')
   Edge line style (solid|dashed|dotted,dashdot)

labels : dictionary, optional (default=None)
   Node labels in a dictionary keyed by node of text labels

font_size : int, optional (default=12)
   Font size for text labels

font_color : string, optional (default='k' black)
   Font color string

font_weight : string, optional (default='normal')
   Font weight

font_family : string, optional (default='sans-serif')
   Font family

label : string, optional
    Label for graph legend

Notes
-----
For directed graphs, arrows  are drawn at the head end.  Arrows can be
turned off with keyword arrows=False.

Examples
--------
>>> G = nx.dodecahedral_graph()
>>> nx.draw(G)
>>> nx.draw(G, pos=nx.spring_layout(G))  # use spring layout

>>> import matplotlib.pyplot as plt
>>> limits = plt.axis('off')  # turn of axis

Also see the NetworkX drawing examples at
https://networkx.github.io/documentation/latest/auto_examples/index.html

See Also
--------
draw()
draw_networkx_nodes()
draw_networkx_edges()
draw_networkx_labels()
draw_networkx_edge_labels()
"""
