import typing as t
import inspect
import asyncio
import handofcats
from tinyrpc.protocols import jsonrpc
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc import exc


class AsyncioRPCDispatcher(RPCDispatcher):
    async def dispatch(self, request):
        if hasattr(request, "create_batch_response"):
            results = await asyncio.gather(
                *[self._dispatch(x) for x in request], return_exceptions=True
            )

            response = request.create_batch_response()
            if response is not None:
                response.extend(results)

            return response
        else:
            return await self._dispatch(request)

    async def _dispatch(self, request):
        try:
            method = self.get_method(request.method)
        except exc.MethodNotFoundError as e:
            return request.error_respond(e)
        except Exception:
            return request.error_respond(exc.ServerError())

        try:
            if self.validator is not None:
                self.validator(method, request.args, request.kwargs)

            if inspect.iscoroutinefunction(method):
                result = await method(*request.args, **request.kwargs)
            else:
                result = method(*request.args, **request.kwargs)
        except Exception as e:
            return request.error_respond(e)

        return request.respond(result)


protocol = jsonrpc.JSONRPCProtocol()
dispatcher = AsyncioRPCDispatcher()


@dispatcher.public
async def add(x: int, y: int) -> t.Dict[str, t.Any]:
    ans = x + y
    await asyncio.sleep(0.5)
    import random

    if random.random() > 0.5:
        raise Exception("oops")
    return {"q": f"{x} + {y} = ?", "a": ans}


@handofcats.as_subcommand
def run_sequentially():
    import logging
    import contextlib

    async def do_task():
        req = protocol.create_request("add", args=[10, 20], kwargs={})
        res = await dispatcher.dispatch(req)
        return res.serialize()

    async def run():
        print(await do_task())
        print(await do_task())
        print(await do_task())

    with contextlib.ExitStack() as s:
        logging.info("start")
        s.callback(logging.info, "end")

        asyncio.run(run(), debug=True)


@handofcats.as_subcommand
def run_concurrently():
    import logging
    import contextlib

    async def run():
        req = protocol.create_batch_request(
            [
                protocol.create_request("add", args=[10, 20], kwargs={}),
                protocol.create_request("add", args=[10, 20], kwargs={}),
                protocol.create_request("add", args=[10, 20], kwargs={}),
            ]
        )
        res = await dispatcher.dispatch(req)
        print([x.serialize() for x in res])

    with contextlib.ExitStack() as s:
        logging.info("start")
        s.callback(logging.info, "end")

        asyncio.run(run(), debug=True)


handofcats.as_subcommand.run()
