from egoist.app import App


def includeme(app: App) -> None:
    app.include("directives.define_server_process")

    from rpcutil import find_free_port, create_sentinel_file

    app.define_server_process(
        "gofmtrpc -addr :{port} -sentinel {sentinel}",
        params=dict(port=find_free_port, sentinel=create_sentinel_file),
        name="gofmtrpc",
    )
