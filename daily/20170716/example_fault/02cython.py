import pyximport
import pyximport.pyximport as x
x.DEBUG_IMPORT = True
pyximport.install()
import fault  # NOQA
print(fault.disp(1000))
