from uvitools.routing import Route, Router


async def hello_world(message, channels):
    data = {'hello': 'world'}
    await channels['reply'].send(
        {
            'status': 200,
            'headers': [[b'content-type', b'application/json']],
            'content': json.dumps(data).encode()
        }
    )


async def hello_user(message, channels):
    data = {'hello': message['args']['username']}
    await channels['reply'].send(
        {
            'status': 200,
            'headers': [[b'content-type', b'application/json']],
            'content': json.dumps(data).encode()
        }
    )


app = Router([Route('/hello/', hello_world), Route('/hello/<username>/', hello_user)])
