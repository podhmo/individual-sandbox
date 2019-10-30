import typesystem

"""
    port:      uint16
    logLevel:  "debug" | *"info" | "warn" | "error" | "critical"
    secondary: <self>
    xxxAPI:
      token: string
"""

definitions = typesystem.SchemaDefinitions()


class XXXAPI(typesystem.Schema):
    name = typesystem.String()


class Config(typesystem.Schema):
    port = typesystem.Integer()
    logLevel = typesystem.Choice(
        title="LogLevel",
        choices=[(x, x) for x in ["debug", "info", "warn", "error", "critical"]],
    )
    # not supported
    # secondary = typesystem.Reference(
    #     to="Config", allow_null=True, definitions=definitions
    # )
    xxxAPI = typesystem.Reference(to=XXXAPI, allow_null=True, definitions=definitions)


definitions["XXXAPI"] = XXXAPI
definitions["Config"] = Config
