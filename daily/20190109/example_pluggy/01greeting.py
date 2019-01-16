import pluggy
from plugins import hello, bye
hookspec = pluggy.HookspecMarker("my")


class Spec:
    @hookspec
    def greeting(msg: str):
        pass


pm = pluggy.PluginManager("my")
pm.enable_tracing()
pm.add_hookspecs(Spec)
# pm.load_setuptools_entrypoints("eggsample")
pm.register(hello)
pm.register(bye)

# TypeError: hook calling supports only keyword arguments
results = pm.hook.greeting(msg="foo")
print(results)
print(pm.subset_hook_caller("greeting", [])(msg="foo"))
# ['bye foo', 'hello foo']
# ['bye foo', 'hello foo']

print([name for name, plugin in pm.list_name_plugin()])
print(pm.list_plugin_distinfo())
# ['plugins.hello', 'plugins.bye']
# []

print(pm.has_plugin("plugins.hello"))
print(pm.get_hookcallers(pm.get_plugin("plugins.hello")))
# True
# [<_HookCaller 'greeting'>]

print([name for name in dir(pm.hook) if not name.startswith("_")])
# ['greeting']
