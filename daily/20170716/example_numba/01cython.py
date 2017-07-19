import pyximport
import pyximport.pyximport as x
x.DEBUG_IMPORT = True
pyximport.install()
import primes  # NOQA
print(primes.primes(1000))
