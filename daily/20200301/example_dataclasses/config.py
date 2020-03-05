import schema

config = schema.Config(
    app=schema.AppConfig(db="sqlite:///:memory:"),
    thirdparty=schema.ThirdpartyConfig(
        foo_token="........................................",
        bar_token="........................................",
    ),
)

if __name__ == "__main__":
    import json
    import dataclasses

    print(json.dumps(dataclasses.asdict(config), indent=2, ensure_ascii=False))
