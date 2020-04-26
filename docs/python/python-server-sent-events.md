# Python Server Sent Events


Server Sent Events (SSE) is a unidirectional connection between a server and a client (usually a web browser) that allows the server to =push= information to the client. It is much like websockets and long polling. The main difference between SSE and websockets is that SSE is unidirectional, only the server can send info to the client, where as with websockets, both can send info to eachother. SSE is typically considered to be much simpler to use/implement than websockets.



## Flask SSE


```
@route(=/stream=)
def stream():
    def event_stream():
        while True:
            if message_to_send:
                yield =data: 
                    {}\n\n=.format(message_to_send)=
    
    return Response(event_stream(), mimetype==text/event-stream=)

```



## Asyncio SSE


This example uses the asyncio SSE library: [https://github.com/brutasse/asyncio-sse](https://github.com/brutasse/asyncio-sse)

```
import asyncio
import sse

class Handler(sse.Handler):
    @asyncio.coroutine
    def handle_request(self):
        yield from asyncio.sleep(2)
        self.send('foo')
        yield from asyncio.sleep(2)
        self.send('bar', event='wakeup')

start_server = sse.serve(Handler, 'localhost', 8888)
asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()

```

