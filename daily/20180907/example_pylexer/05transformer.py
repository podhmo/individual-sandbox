from IPython.core.inputtransformer import escaped_commands

transformer = escaped_commands()

code = "%time None"
print(transformer.push(code))
# get_ipython().run_line_magic('time', 'None')
