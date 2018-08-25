import argparse
import json
import os
import signal
import socket
import sys
import time
import daemon
from typing import Any, Callable, Dict, Mapping, Optional, Tuple
import tempfile
import traceback
import shutil
__version__ = "0.0.0"
STATUS_FILE = "decho.json"
SOCKET_NAME = 'decho.sock'


class Server:
    def __init__(self, *, timeout: Optional[int] = None) -> None:
        self.timeout = timeout

    def create_listening_socket(self) -> socket.socket:
        """Create the socket and set it up for listening."""
        self.sock_directory = tempfile.mkdtemp()
        sockname = os.path.join(self.sock_directory, SOCKET_NAME)
        sock = socket.socket(socket.AF_UNIX)
        sock.bind(sockname)
        sock.listen(1)
        return sock

    def run_command(self, command: str, data: Mapping[str, object]) -> Dict[str, object]:
        """Run a specific command from the registry."""
        key = 'cmd_' + command
        method = getattr(self.__class__, key, None)
        if method is None:
            return {'error': "Unrecognized command '%s'" % command}
        else:
            return method(self, **data)

    def cmd_status(self) -> Dict[str, object]:
        """Return daemon status."""
        res = {}  # type: Dict[str, object]
        res.update(get_meminfo())
        return res

    def cmd_stop(self) -> Dict[str, object]:
        """Stop daemon."""
        return {}

    def cmd_ping(self) -> Dict[str, object]:
        return {"message": "pong"}

    def serve(self) -> None:
        """Serve requests, synchronously (no thread or fork)."""
        try:
            sock = self.create_listening_socket()
            if self.timeout is not None:
                sock.settimeout(self.timeout)
            try:
                with open(STATUS_FILE, 'w') as f:
                    json.dump({'pid': os.getpid(), 'sockname': sock.getsockname()}, f)
                    f.write('\n')  # I like my JSON with trailing newline
                while True:
                    try:
                        conn, addr = sock.accept()
                    except socket.timeout:
                        print("Exiting due to inactivity.")
                        # reset_global_state()
                        sys.exit(0)
                    try:
                        data = receive(conn)
                    except OSError as err:
                        conn.close()  # Maybe the client hung up
                        continue
                    resp = {}  # type: Dict[str, Any]
                    if 'command' not in data:
                        resp = {'error': "No command found in request"}
                    else:
                        command = data['command']
                        if not isinstance(command, str):
                            resp = {'error': "Command is not a string"}
                        else:
                            command = data.pop('command')
                            try:
                                resp = self.run_command(command, data)
                            except Exception:
                                # If we are crashing, report the crash to the client
                                tb = traceback.format_exception(*sys.exc_info())
                                resp = {'error': "Daemon crashed!\n" + "".join(tb)}
                                conn.sendall(json.dumps(resp).encode('utf8'))
                                raise
                    try:
                        conn.sendall(json.dumps(resp).encode('utf8'))
                    except OSError as err:
                        pass  # Maybe the client hung up
                    conn.close()
                    if command == 'stop':
                        sock.close()
                        # reset_global_state()
                        sys.exit(0)
            finally:
                os.unlink(STATUS_FILE)
        finally:
            shutil.rmtree(self.sock_directory)
            exc_info = sys.exc_info()
            if exc_info[0] and exc_info[0] is not SystemExit:
                traceback.print_exception(*exc_info)


def receive(sock: socket.socket) -> Any:
    """Receive JSON data from a socket until EOF.

    Raise a subclass of OSError if there's a socket exception.

    Raise OSError if the data received is not valid JSON or if it is
    not a dict.
    """
    bdata = bytearray()
    while True:
        more = sock.recv(100000)
        if not more:
            break
        bdata.extend(more)
    if not bdata:
        raise OSError("No data received")
    try:
        data = json.loads(bdata.decode('utf8'))
    except Exception:
        raise OSError("Data received is not valid JSON")
    if not isinstance(data, dict):
        raise OSError("Data received is not a dict (%s)" % str(type(data)))
    return data


# Argument parser.  Subparsers are tied to action functions by the
# @action(subparse) decorator.

parser = argparse.ArgumentParser(
    description="Client for echo daemon mode", fromfile_prefix_chars='@'
)
parser.set_defaults(action=None)
subparsers = parser.add_subparsers()

start_parser = p = subparsers.add_parser('start', help="Start daemon")
p.add_argument(
    '--timeout', metavar='TIMEOUT', type=int, help="Server shutdown timeout (in seconds)"
)

status_parser = p = subparsers.add_parser('status', help="Show daemon status")
p.add_argument('-v', '--verbose', action='store_true', help="Print detailed status")

stop_parser = p = subparsers.add_parser('stop', help="Stop daemon (asks it politely to go away)")

kill_parser = p = subparsers.add_parser('kill', help="Kill daemon (kills the process)")
ping_parser = p = subparsers.add_parser('ping', help="Ping")

run_parser = p = subparsers.add_parser(
    'run', help="Check some files, [re]starting daemon if necessary"
)
p.add_argument('-v', '--verbose', action='store_true', help="Print detailed status")
p.add_argument(
    '--timeout', metavar='TIMEOUT', type=int, help="Server shutdown timeout (in seconds)"
)

help_parser = p = subparsers.add_parser('help')

del p


class BadStatus(Exception):
    """Exception raised when there is something wrong with the status file.

    For example:
    - No status file found
    - Status file malformed
    - Process whose pid is in the status file does not exist
    """
    pass


def main() -> None:
    """The code is top-down."""
    args = parser.parse_args()
    if not args.action:
        parser.print_usage()
    else:
        try:
            args.action(args)
        except BadStatus as err:
            sys.exit(err.args[0])


ActionFunction = Callable[[argparse.Namespace], None]


def action(subparser: argparse.ArgumentParser) -> Callable[[ActionFunction], ActionFunction]:
    """Decorator to tie an action function to a subparser."""

    def register(func: ActionFunction) -> ActionFunction:
        subparser.set_defaults(action=func)
        return func

    return register


# Action functions (run in client from command line).


@action(start_parser)
def do_start(args: argparse.Namespace) -> None:
    try:
        get_status()
    except BadStatus as err:
        # Bad or missing status file or dead process; good to start.
        pass
    else:
        sys.exit("Daemon is still alive")
    start_server(args)


def start_server(args: argparse.Namespace, allow_sources: bool = False) -> None:
    """Start the server from command arguments and wait for it."""
    # Lazy import so this import doesn't slow down other commands.
    with daemon.DaemonContext(
        working_directory=".",
        stdout=open("./decho.stdout.log", "w+"),
        stderr=open("./decho.stderr.log", "w+"),
    ):
        Server(timeout=args.timeout).serve()
        wait_for_server()


def restart_server(args: argparse.Namespace, allow_sources: bool = False) -> None:
    """Restart daemon (it may or may not be running; but not hanging)."""
    try:
        do_stop(args)
    except BadStatus:
        # Bad or missing status file or dead process; good to start.
        pass
    start_server(args, allow_sources)


def wait_for_server(timeout: float = 5.0) -> None:
    """Wait until the server is up.

    Exit if it doesn't happen within the timeout.
    """
    endtime = time.time() + timeout
    while time.time() < endtime:
        try:
            data = read_status()
        except BadStatus:
            # If the file isn't there yet, retry later.
            time.sleep(0.1)
            continue
        # If the file's content is bogus or the process is dead, fail.
        pid, sockname = check_status(data)
        print("Daemon started")
        return
    sys.exit("Timed out waiting for daemon to start")


@action(run_parser)
def do_run(args: argparse.Namespace) -> None:
    """Do a check, starting (or restarting) the daemon as necessary

    Restarts the daemon if the running daemon reports that it is
    required (due to a configuration change, for example).

    Setting flags is a bit awkward; you have to use e.g.:

      dmypy run -- --strict a.py b.py ...

    since we don't want to duplicate mypy's huge list of flags.
    (The -- is only necessary if flags are specified.)
    """
    if not is_running():
        # Bad or missing status file or dead process; good to start.
        start_server(args, allow_sources=True)

    t0 = time.time()
    response = request('run', version=__version__, args=args.flags)
    # If the daemon signals that a restart is necessary, do it
    if 'restart' in response:
        print('Restarting: {}'.format(response['restart']))
        restart_server(args, allow_sources=True)
        response = request('run', version=__version__, args=args.flags)

    t1 = time.time()
    response['roundtrip_time'] = t1 - t0


@action(status_parser)
def do_status(args: argparse.Namespace) -> None:
    """Print daemon status.

    This verifies that it is responsive to requests.
    """
    status = read_status()
    if args.verbose:
        show_stats(status)
    # Both check_status() and request() may raise BadStatus,
    # which will be handled by main().
    check_status(status)
    response = request('status', timeout=5)
    if args.verbose or 'error' in response:
        show_stats(response)
    if 'error' in response:
        sys.exit("Daemon is stuck; consider %s kill" % sys.argv[0])
    print("Daemon is up and running")


@action(stop_parser)
def do_stop(args: argparse.Namespace) -> None:
    """Stop daemon via a 'stop' request."""
    # May raise BadStatus, which will be handled by main().
    response = request('stop', timeout=5)
    if response:
        show_stats(response)
        sys.exit("Daemon is stuck; consider %s kill" % sys.argv[0])
    else:
        print("Daemon stopped")


@action(kill_parser)
def do_kill(args: argparse.Namespace) -> None:
    """Kill daemon process with SIGKILL."""
    pid, sockname = get_status()
    try:
        os.kill(pid, signal.SIGKILL)
    except OSError as err:
        sys.exit(str(err))
    else:
        print("Daemon killed")


def show_stats(response: Mapping[str, object]) -> None:
    for key, value in sorted(response.items()):
        if key not in ('out', 'err'):
            print("%-24s: %10s" % (key, "%.3f" % value if isinstance(value, float) else value))
        else:
            value = str(value).replace('\n', '\\n')
            if len(value) > 50:
                value = value[:40] + ' ...'
            print("%-24s: %s" % (key, value))


@action(ping_parser)
def do_ping(args: argparse.Namespace) -> None:
    print(request('ping', timeout=1))


@action(help_parser)
def do_help(args: argparse.Namespace) -> None:
    """Print full help (same as dmypy --help)."""
    parser.print_help()


# Client-side infrastructure.


def request(command: str, *, timeout: Optional[float] = None, **kwds: object) -> Dict[str, Any]:
    """Send a request to the daemon.

    Return the JSON dict with the response.

    Raise BadStatus if there is something wrong with the status file
    or if the process whose pid is in the status file has died.

    Return {'error': <message>} if a socket operation or receive()
    raised OSError.  This covers cases such as connection refused or
    closed prematurely as well as invalid JSON received.
    """
    args = dict(kwds)
    args.update(command=command)
    bdata = json.dumps(args).encode('utf8')
    pid, sockname = get_status()
    sock = socket.socket(socket.AF_UNIX)
    if timeout is not None:
        sock.settimeout(timeout)
    try:
        sock.connect(sockname)
        sock.sendall(bdata)
        sock.shutdown(socket.SHUT_WR)
        response = receive(sock)
    except OSError as err:
        return {'error': str(err)}
    # TODO: Other errors, e.g. ValueError, UnicodeError
    else:
        return response
    finally:
        sock.close()


def get_status() -> Tuple[int, str]:
    """Read status file and check if the process is alive.

    Return (pid, sockname) on success.

    Raise BadStatus if something's wrong.
    """
    data = read_status()
    return check_status(data)


def check_status(data: Dict[str, Any]) -> Tuple[int, str]:
    """Check if the process is alive.

    Return (pid, sockname) on success.

    Raise BadStatus if something's wrong.
    """
    if 'pid' not in data:
        raise BadStatus("Invalid status file (no pid field)")
    pid = data['pid']
    if not isinstance(pid, int):
        raise BadStatus("pid field is not an int")
    try:
        os.kill(pid, 0)
    except OSError as err:
        raise BadStatus("Daemon has died")
    if 'sockname' not in data:
        raise BadStatus("Invalid status file (no sockname field)")
    sockname = data['sockname']
    if not isinstance(sockname, str):
        raise BadStatus("sockname field is not a string")
    return pid, sockname


def read_status() -> Dict[str, object]:
    """Read status file.

    Raise BadStatus if the status file doesn't exist or contains
    invalid JSON or the JSON is not a dict.
    """
    if not os.path.isfile(STATUS_FILE):
        raise BadStatus("No status file found")
    with open(STATUS_FILE) as f:
        try:
            data = json.load(f)
        except Exception as err:
            raise BadStatus("Malformed status file (not JSON)")
    if not isinstance(data, dict):
        raise BadStatus("Invalid status file (not a dict)")
    return data


def is_running() -> bool:
    """Check if the server is running cleanly"""
    try:
        get_status()
    except BadStatus as err:
        return False
    return True


# Misc utilities.

MiB = 2 ** 20


def get_meminfo() -> Dict[str, Any]:
    # See https://stackoverflow.com/questions/938733/total-memory-used-by-python-process
    import resource  # Since it doesn't exist on Windows.
    res = {}  # type: Dict[str, Any]
    rusage = resource.getrusage(resource.RUSAGE_SELF)
    if sys.platform == 'darwin':
        factor = 1
    else:
        factor = 1024  # Linux
    res['memory_maxrss_mib'] = rusage.ru_maxrss * factor / MiB
    # If we can import psutil, use it for some extra data
    try:
        import psutil  # type: ignore  # It's not in typeshed yet
    except ImportError:
        if sys.platform != 'win32':
            res['memory_psutil_missing'] = (
                'psutil not found, run pip install mypy[dmypy] '
                'to install the needed components for dmypy'
            )
    else:
        process = psutil.Process(os.getpid())
        meminfo = process.memory_info()
        res['memory_rss_mib'] = meminfo.rss / MiB
        res['memory_vms_mib'] = meminfo.vms / MiB
    return res


# Run main().

if __name__ == '__main__':
    main()
