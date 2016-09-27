from subprocess import PIPE
import asyncio
import logging
logger = logging.getLogger(__name__)


@asyncio.coroutine
def check_status(p):
    if p.returncode:
        stdout = yield from p.stdout.read()
        stderr = yield from p.stderr.read()
        logger.warn(
            "error on pid=%s, returncode=%s, stdout=%s, stderr=%s",
            p.pid, p.returncode, stdout, stderr
        )
        return False
    return True


@asyncio.coroutine
def main():
    cmds = ["ls", "lls"]
    for cmd in cmds:
        logger.info(cmd)
        p = yield from asyncio.create_subprocess_shell(cmd, stderr=PIPE, stdout=PIPE)
        yield from p.wait()
        ok = yield from check_status(p)
        if ok:
            stdout = yield from p.stdout.read()
            logger.info("output on pid=%s, %s", p.pid, stdout)

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)s\t%(asctime)s\t%(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(main())
    loop.close()
