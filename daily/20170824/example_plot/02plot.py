from utatane import as_command

L = [
    1089536,
    1048576,
    1073152,
    1040384,
    1253376,
    1171456,
    1196032,
    1163264,
    1196032,
    1056768,
    1097728,
    1064960,
]


@as_command
def render(plt):
    plt.plot(L)
