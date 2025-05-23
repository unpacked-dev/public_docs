# Deno websocket example
A simple example connecting deno to a browser via websockets.

## 🚀 Getting started
### Create your server `app.js`

```js
const CONFIG = {
    port: 8080 //Specify your port
}

Deno.serve({ port: CONFIG.port }, (request) => {
    const { socket, response } = Deno.upgradeWebSocket(request);

    socket.onopen = () => {
        console.log("🟢 Opened websocketconnection");
    };

    socket.onmessage = async (event) => {
        const message = event.data;
        console.log(`📨 Recieved message: ${message}`);
    };

    socket.onclose = () => {
        console.log("🔴 Closed websocketconnection")
    };

    return response;
});

console.log(`🚀 Server running on http://localhost:${CONFIG.port}`);
```
**Start your server:** `Deno run --allow-net ./app.js`

---

### Create your website `index.html`

```html
<!DOCTYPE html>
<html lang="de">

<head>
    <meta charset="UTF-8" />
</head>

<body>
    <pre>
        Check developer console. (F12)
        - Reconnect your websocket "websocket_reconnect();"
        - Send messages "websocket_send_message(message);"
    </pre>

    <script>
        let websocket = websocket_create();

        function websocket_create() {
            const ws = new WebSocket("ws://localhost:8080/");

            ws.onopen = () => {
                console.log(`🟢 Opened websocketconnection`);
            };

            ws.onmessage = async (event) => {
                const message = event.data;
                console.log(`📨 Recieved message: ${message}`);
            };

            ws.onclose = () => {
                console.log(`🔴 Closed websocketconnection`);
                websocket = null;
            };

            return ws;
        }

        function websocket_reconnect() {
            console.log(`🚧 Trying to reconnect...`);
            websocket = websocket_create();
        }

        function websocket_send_message(message) {
            if(!websocket) {
                console.log(`🔴 Websocket not connected!`);
                return;
            }

            websocket.send(message);
            console.log(`📨 Sent message: ${message}`);
        }
    </script>
</body>

</html>
```

--- 
© unpacked - [licence](../../LICENSE)