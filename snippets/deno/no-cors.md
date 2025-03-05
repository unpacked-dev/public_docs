# No-Cors
A simple deno server-snippet to fetch content from any url without being restricted by cors.
This can be useful for local development or as a basic proxy.

## Getting Started
### Creating your sever file 

`app.js`
```js
import { Application, Router } from "https://deno.land/x/oak@v12.5.0/mod.ts";
import { oakCors } from "https://deno.land/x/cors@v1.2.2/mod.ts";

const port = 8000;
const app = new Application();
const router = new Router();

app.use(oakCors({ origin: "*" }));
app.use(router.routes());
app.use(router.allowedMethods());

app.addEventListener("listen", () => {
    console.log(`Server running on port ${port}`);
});

router.get("/:url", async (ctx) => {
    try {
        const targetUrl = decodeURIComponent(ctx.params.url);
        const res = await fetch(targetUrl);

        ctx.response.status = res.status;
        res.headers.forEach((value, key) => {
            if (key.toLowerCase() !== "content-encoding") {
                ctx.response.headers.set(key, value);
            }
        });
        ctx.response.body = res.body;
    } 
    catch (error) {
        console.error("Proxy error:", error);
        ctx.response.status = 500;
        ctx.response.body = "Proxy error";
    }
});
on_startup();

async function on_startup() {
    await app.listen({ port });
}
```

## Start the server
Start the server with this simple command: `deno run --allow-net app.js`

## Making requests
1. URL-Encode your target url
2. Do `GET` request to `http://localhost:8000/YOUR_TARGET_URL`

---
© unpacked - [licence](../../LICENSE)