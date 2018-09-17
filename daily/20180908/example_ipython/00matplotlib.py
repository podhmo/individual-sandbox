from IPython import embed

config = {"header": "hi, "}
xs = [1, 2, 3, 4]
embed(**config)
# jupyterなどで立ち上げていないと
# UnknownBackend: No event loop integration for 'inline'. Supported event loops are: qt, qt4, qt5, gtk, gtk2, gtk3, tk, wx, pyglet, glut, osx
