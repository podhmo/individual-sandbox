from handofcats import as_command


@as_command
def run(*, inputname: str, outputname: str) -> None:
    from papermill.parameterize import parameterize_notebook
    from papermill.iorw import load_notebook_node, write_ipynb

    nb = load_notebook_node(inputname)
    nb = parameterize_notebook(nb, parameters={"data": "yyy"})
    write_ipynb(nb, outputname)
