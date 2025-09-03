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
    return reg[key] ||= { resolved: false, value: undefined, promise: null, resolve: null };
  }
  w.when_ready = function (key) {
    const slot = getSlot(key);
    if (slot.resolved) {
      return Promise.resolve(slot.value);
    }
    if (!slot.promise) {
      slot.promise = new Promise(res => { slot.resolve = res; });
      slot.timeoutId = setTimeout(() => {
        if (!slot.resolved) {
          console.warn(`[when_ready] '${key}' wurde nach 60s nicht resolved.`);
        }
      }, 60000);
    }
    return slot.promise;
  };
  w.signal_ready = function (key, value) {
    const slot = getSlot(key);
    if (slot.resolved) {
        return;
    }
    slot.resolved = true;
    slot.value = value;
    if (slot.resolve) {
      slot.resolve(value);
    }
  };
  w.reset_ready = function (key) {
    reg[key] = { resolved: false, value: undefined, promise: null, resolve: null };
  };
})(window);
```

## ⌨️ Examples
```js
//Define your event handler
when_ready("plugin:slider").then(() => { 
    init_slider(); 
});

//Trigger your event (works best at the end of the plugin file)
signal_ready(`plugin:slider`);
```

--- 
© unpacked - [licence](../../LICENSE)