from utatane import as_command

L = [
    1466368,
    1605632,
    2375680,
    1875968,
    2908160,
    1990656,
    2818048,
    1990656,
    2842624,
    1908736,
    2768896,
    1908736,
]


@as_command
def render(plt):
    plt.plot(L)
