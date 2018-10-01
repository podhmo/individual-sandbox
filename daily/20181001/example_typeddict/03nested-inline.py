import mypy_extensions as mx

# error
# Person = mx.TypedDict(
#     "Person", {
#         "name": str,
#         "age": int,
#         "info": mx.TypedDict("Info",
#                              {"name": str})
#     }
# )
Info = mx.TypedDict("Info", {"name": str})
Person = mx.TypedDict("Person", {
    "name": str,
    "age": int,
    "info": Info,
})
