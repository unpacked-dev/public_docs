# Vanilla JS Ready-Handler for (async) Events

Sometimes scripts, plugins or imports need to be loaded asynchronously –  
but that often leads to timing issues:  
*How do you run code only after a certain dependency is really available?*

This tiny utility provides a **Promise-based ready registry** to coordinate async events in plain JavaScript.  
It works without frameworks and avoids race conditions by letting you wait until a named "ready signal" is triggered.

## 🚀 Getting started
Copy the following definition into your code.
```js
(function (w) {
    const reg = w.__readyRegistry ||= {};

    function getSlot(key) {
        return reg[key] ||= {
            resolved: false,
            value: undefined,
            ctx: undefined,
            promise: null,
            resolve: null,
            listeners: []
        };
    }

    // one-time signal
    w.await_ready = function (key) {
        const slot = getSlot(key);
        if (slot.resolved) {
            return Promise.resolve({ ctx: slot.ctx, value: slot.value });
        }
        if (!slot.promise) {
            slot.promise = new Promise(res => { slot.resolve = res; });
            slot.timeoutId = setTimeout(() => {
                if (!slot.resolved) {
                    console.warn(`[await_ready] '${key}' wurde nach 60s nicht resolved.`);
                }
            }, 60000);
        }
        return slot.promise;
    };

    // push signal
    w.signal_ready = function (key, ctx, value) {
        const slot = getSlot(key);
        slot.resolved = true;
        slot.value = value;
        slot.ctx = ctx;

        const payload = { value, ctx };

        // one-time promise
        if (slot.resolve) {
            slot.resolve(payload);
            slot.resolve = null;
            slot.promise = null;
        }

        // permanent promise
        if (slot.listeners.length > 0) {
            slot.listeners.forEach(fn => {
                try { fn.call(ctx || null, payload); } catch (e) { console.error(e); }
            });
        }
    };

    // permanent listener
    w.subscribe_ready = function (key, fn) {
        const slot = getSlot(key);
        slot.listeners.push(fn);

        if (slot.resolved) {
            const payload = { ctx: slot.ctx, value: slot.value };
            try { fn.call(slot.ctx || null, payload); } catch (e) { console.error(e); }
        }
    };

    w.unsubscribe_ready = function (key, fn) {
        const slot = getSlot(key);
        slot.listeners = slot.listeners.filter(f => f !== fn);
    };

    w.reset_ready = function (key) {
        reg[key] = {
            resolved: false,
            value: undefined,
            ctx: undefined,
            promise: null,
            resolve: null,
            listeners: []
        };
    };

})(window);
```

## ⌨️ Examples
### One time listener (runs only FIRST TIME event is recieved)
```js
await_ready("plugin:slider").then(() => { 
    init_slider(); 
});

//Note: Can be reset with reset_ready("plugin:slider");
```

### Permanent listener (runs EVERY TIME event is recieved)
```js
subscribe_ready("ping", () => {
  console.log("pong");
}

//Note: Can be disabled with unsubscribe_ready("ping");
```

### Trigger Events
```js
signal_ready(`plugin:slider`);
```

--- 

© unpacked - [licence](../../LICENSE)
