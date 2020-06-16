from egoist.app import create_app, SettingsDict, App

settings: SettingsDict = {"rootdir": "output", "here": __file__}
app = create_app(settings)


def setup_server(app: App) -> None:
    from egoist.ext.serverprocess.lazyparams import find_free_port, create_sentinel_file

    app.include("egoist.ext.serverprocess")  # for add_server_process

    app.add_server_process(
        "uvicorn nowait:app --port {port}",
        params=dict(port=find_free_port),
        name="api-server",
        nowait=True,
    )


def run() -> None:
    from egoist.ext.serverprocess.runtime import get_discovery, get_http_client

    import time
    time.sleep(0.5)
    url = get_discovery().lookup("api-server")
    print("->", url)
    res = get_http_client().get(url)
    res.raise_for_status()
    print("<-", res.json())


if __name__ == "__main__":
    app.include(setup_server)
    app.commit(dry_run=False)
    run()
