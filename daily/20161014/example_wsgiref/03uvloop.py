import uvloop
import asyncio
import magicalimport
asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
magicalimport.import_from_physical_path("./02aiohello.py")
