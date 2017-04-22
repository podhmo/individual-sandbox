from zenmai.decorators import with_context


@with_context
def get(name, context):
    return getattr(context.scope, name)


def odds(nums):
    return [n for n in nums if n % 2 == 1]


def evens(nums):
    return [n for n in nums if n % 2 == 0]
