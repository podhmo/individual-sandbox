# only my venv
import zope.interface
# only viz venv
import seaborn

"""
epc con 11      open    *epc con 11*    --           (network connection to localhost)
epc con 3       open    *epc con 3*     --           (network connection to localhost)
epc:server:10   run      *epc:server... /dev/pts/9   $HOME/.emacs.d/.python-environments/default/bin/jediepcserver --log-level DEBUG --virtual-env $HOME/venvs/viz/
epc:server:2    run      *epc:server:2* /dev/pts/8   $HOME/.emacs.d/.python-environments/default/bin/jediepcserver --log-level DEBUG --virtual-env $HOME/venvs/my/
"""
