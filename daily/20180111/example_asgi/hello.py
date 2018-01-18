async def hello_world(message, channels):
    content = b'Hello, world'
    response = {
        'status': 200,
        'headers': [
            [b'content-type', b'text/plain'],
        ],
        'content': content
    }
    await channels['reply'].send(response)
