import jedi

source = '''\
import sys
sys.exi'''
script = jedi.Script(source, 2, len('sys.exi'), 'example.py')
script.completions() # => [<Completion: exit>]
