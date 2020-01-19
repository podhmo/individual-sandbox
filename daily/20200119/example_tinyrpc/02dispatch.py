from tinyrpc.dispatch import RPCDispatcher

dispatcher = RPCDispatcher()


@dispatcher.public
def add(x: int, y: int) -> int:
    return x + y


dispatcher.get_method("add")  # => <function add at 0x101f0e700>
dispatcher.get_method("add")(10, 20)  # => 30

dispatcher.validator  # => <function RPCDispatcher.validate_parameters at 0x101f6baf0>
dispatcher.validator(add, args=[10, 20], kwargs={})

RPCDispatcher.validate_parameters(add, args=[10, 20], kwargs={})
