from IPython.core.inputsplitter import IPythonInputSplitter

spliter = IPythonInputSplitter()
cell = "%time None"
print(spliter.transform_cell(cell))
# get_ipython().run_line_magic('time', 'None')
