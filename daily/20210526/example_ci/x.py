import sys
from datetime import datetime


def run(s):
    lines = s.splitlines()
    lhs, rhs = lines[0].split("\t", 1)
    dt = datetime.strptime(lhs[:19], "%Y-%m-%dT%H:%M:%S")
    for line in lines[1:]:
        prev_rhs = rhs
        lhs, rhs = line.split("\t", 1)
        prev, dt = dt, datetime.strptime(lhs[:19], "%Y-%m-%dT%H:%M:%S")
        print("{:6}\t{}".format((dt - prev).seconds, prev_rhs))
    print("{:6}\t{}".format(0, rhs))


LINES = s = """\
2021-05-26T04:49:32.9357292Z	lint debug ubuntu-latest-xl	Set up job
2021-05-26T04:49:35.6004186Z	lint debug ubuntu-latest-xl	Configure git
2021-05-26T04:49:35.6359802Z	lint debug ubuntu-latest-xl	Clone repository
2021-05-26T04:50:08.7228231Z	lint debug ubuntu-latest-xl	Install rust
2021-05-26T04:50:17.1333213Z	lint debug ubuntu-latest-xl	Install clippy and rustfmt
2021-05-26T04:50:18.3392207Z	lint debug ubuntu-latest-xl	Install Deno
2021-05-26T04:50:19.4309392Z	lint debug ubuntu-latest-xl	Error on Warning
2021-05-26T04:50:19.4493111Z	lint debug ubuntu-latest-xl	Install Python
2021-05-26T04:50:19.5125359Z	lint debug ubuntu-latest-xl	Install Node
2021-05-26T04:50:22.2705961Z	lint debug ubuntu-latest-xl	Log versions
2021-05-26T04:50:22.5348361Z	lint debug ubuntu-latest-xl	Cache Cargo home
2021-05-26T04:50:23.6385170Z	lint debug ubuntu-latest-xl	Cache build output (PR)
2021-05-26T04:50:39.7181592Z	lint debug ubuntu-latest-xl	Skip save cache (PR)
2021-05-26T04:50:39.7373630Z	lint debug ubuntu-latest-xl	Apply and update mtime cache
2021-05-26T04:50:47.5859611Z	lint debug ubuntu-latest-xl	test_format.js
2021-05-26T04:50:52.8218664Z	lint debug ubuntu-latest-xl	lint.js
2021-05-26T04:51:40.1964640Z	lint debug ubuntu-latest-xl	Configure hosts file for WPT (linux)
2021-05-26T04:51:40.8074121Z	lint debug ubuntu-latest-xl	Clean before cache
2021-05-26T04:51:40.8850559Z	lint debug ubuntu-latest-xl	Post Cache Cargo home
2021-05-26T04:51:41.0161551Z	lint debug ubuntu-latest-xl	Post Clone repository
2021-05-26T04:51:41.2773725Z	lint debug ubuntu-latest-xl	Complete job
"""


if sys.stdin.isatty():
    run(LINES)
else:
    run(sys.stdin.read())
