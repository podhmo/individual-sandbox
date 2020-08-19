from egoist.app import create_app, SettingsDict, App

settings: SettingsDict = {"rootdir": "output", "here": __file__}
app = create_app(settings)


def setup_server(app: App) -> None:
    import pathlib
    from egoist.ext.serverprocess.lazyparams import find_free_port, create_sentinel_file

    app.include("egoist.ext.serverprocess")  # for add_server_process
    app.add_server_process(
        "go run main.go",
        name="api-server",
        cwd=pathlib.Path(__file__).parent,
        env={"SENTINEL": create_sentinel_file, "PORT": find_free_port},
    )


def run() -> None:
    from egoist.ext.serverprocess.runtime import get_discovery, get_http_client

    url = get_discovery().lookup("api-server")
    print("->", url)
    res = get_http_client().get(url + "/")
    res.raise_for_status()
    print("<-", res.text)


if __name__ == "__main__":
    # import logging

    # logging.basicConfig(level=logging.DEBUG)
    app.include(setup_server)
    app.commit(dry_run=False)
    run()
