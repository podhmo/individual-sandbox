import itertools
import time

frames = itertools.cycle(["◰", "◳", "◲", "◱"])
frames2 = itertools.cycle(["🌑 ", "🌒 ", "🌓 ", "🌔 ", "🌕 ", "🌖 ", "🌗 ", "🌘 "])
frames3 = itertools.cycle(
    ["🕛 ", "🕐 ", "🕑 ", "🕒 ", "🕓 ", "🕔 ", "🕕 ", "🕖 ", "🕗 ", "🕘 ", "🕙 ", "🕚 "]
)
frames4 = itertools.cycle(["🚶 ", "🏃 "])
frames5 = itertools.cycle(["💛 ", "💙 ", "💜 ", "💚 ", "❤️ "])

for i, frame, frame2, frame3, frame4, frame5 in zip(
    range(10), frames, frames2, frames3, frames4, frames5
):
    print(f"\033[1F\033[J{frame}  {frame2}  {frame3}  {frame4}  {frame5}")
    time.sleep(0.5)
