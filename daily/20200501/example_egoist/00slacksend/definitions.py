from __future__ import annotations
import typing as t
from egoist import runtime
from egoist.go.types import gopackage, GoError, GoPointer


def _to_str(s: str) -> str:
    from prestring.utils import UnRepr
    import json

    return UnRepr(json.dumps(s))


@gopackage("m/config")
class config_pkg:
    class Config:
        pass

    @staticmethod
    def LoadConfig(config_file: str) -> t.Tuple[GoPointer[config_pkg.Config], GoError]:
        pass


@gopackage("m/api")
class api_pkg:
    class Client:
        pass

    @staticmethod
    def NewClient(
        config: GoPointer[config_pkg.Config],
    ) -> t.Tuple[GoPointer[api_pkg.Client], GoError]:
        pass

    class API:
        pass

    @staticmethod
    def NewAPI(client: GoPointer[api_pkg.Client]) -> GoPointer[api_pkg.API]:
        pass


def slacksend(*, config: str) -> GoError:
    """send message to slack channel"""
    from egoist.generate.clikit import clikit
    from egoist.go import di

    with runtime.generate(clikit) as m:
        context = m.import_("context")
        ctx = m.let("ctx", context.Background())

        b = di.Builder()
        b.add_provider(config_pkg.LoadConfig)
        b.add_provider(api_pkg.NewClient)
        b.add_provider(api_pkg.NewAPI)

        injector = b.build(variables={"config_file": config})
        api = injector.inject(m)

        args = runtime.get_cli_rest_args()
        with m.for_(f"_, text := range {args}"):
            with m.if_(
                "err := {}; err != nil",
                api.Send(ctx, m.symbol("text"), _to_str("random")),
            ):
                m.return_("err")
        m.return_("nil")


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="cmd")
