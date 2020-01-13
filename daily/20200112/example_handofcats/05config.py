from handofcats import Config, as_subcommand


@as_subcommand
def foo():
    pass


cfg = Config(ignore_logging=True)
if __name__ == "__main__":
    as_subcommand.run(config=cfg)
