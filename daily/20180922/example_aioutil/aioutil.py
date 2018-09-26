import asyncio
import contextlib
from functools import partial


class reify(object):
    """cached property"""

    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except Exception:
            pass

    def __get__(self, inst, objtype=None):
        if inst is None:
            return self
        val = self.wrapped(inst)
        setattr(inst, self.wrapped.__name__, val)
        return val


class _LazyAsyncContextManager:
    def __init__(self, fn):
        self.fn = fn
        self.actx = None

    def __getattr__(self, name):
        return _LazyMethod(self, name)

    def _bind(self, actx):
        self.actx = actx


class _LazyMethod:
    def __init__(self, manager, name):
        self.manager = manager
        self.name = name

    def __call__(self, *args, **kwargs):
        fn = getattr(self.manager.actx, self.name)
        return fn(*args, **kwargs)

    def __repr__(self):
        return f"<Lazy {self.manager.actx!r}, {self.name!r}>"


class Group:
    def __init__(self, *, loop=None, limit=None, middleware=None):
        self.tasks = []

        if loop is not None:
            self.loop = loop

        self.middleware = middleware or []
        if limit is not None:
            self.middleware.append(asyncio.Semaphore(limit))

    @reify
    def loop(self):
        return asyncio.get_event_loop()

    def go(self, fn, *args, **kwargs):
        return self.tasks.append(("task", fn, args, kwargs))

    @contextlib.contextmanager
    def __call__(self, fn, *args, **kwargs):
        lazy_actx = _LazyAsyncContextManager(fn)
        self.tasks.append(("actx", lazy_actx, args, kwargs))
        yield lazy_actx

    def wait(self):
        async def run():
            tasks = []
            async with contextlib.AsyncExitStack() as s:
                for kind, lazy_actx, args, kwargs in self.tasks:
                    if kind == "actx":
                        actx = lazy_actx.fn(*args, **kwargs)
                        await s.enter_async_context(actx)
                        lazy_actx._bind(actx)

                for kind, fn, args, kwargs in self.tasks:
                    if kind == "task":
                        tasks.append(self._create_task(fn, args, kwargs))
                return await asyncio.gather(*tasks, loop=self.loop)

        return self.loop.run_until_complete(run())

    async def _create_task(self, fn, args, kwargs):
        async with contextlib.AsyncExitStack() as s:
            for actx in self.middleware:
                await s.enter_async_context(actx)

            if asyncio.iscoroutinefunction(fn):
                return await fn(*args, **kwargs)
            else:
                fn = partial(fn, *args, **kwargs)
                return await self.loop.run_in_executor(None, fn)
