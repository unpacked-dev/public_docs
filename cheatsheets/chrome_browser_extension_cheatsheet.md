# Blueprint for a Chromium based browser extension
A small basic blueprint for creating browser extensions for Chromium based browsers.

## Create directory tree
Create the files and folders as shown below:
```
Extension/
    ├ manifest.json
    ├ inject.js
    └ assets/
        ├ logo.png
        └ script.js
```

## Files
`manifest.json`
```json
{
    "name": "NAME",
    "description": "DESCRIPTION",
    "version": "1.0",
    "manifest_version": 3,
    "icons": {
      "64": "assets/logo.png"
    },
    "content_scripts": [{
      "js": ["./inject.js"],
      "matches": ["https://*/*"]
    }],
    "web_accessible_resources": [{
        "resources": ["*.js", "*.json"],
        "matches": ["https://*/*"]
    }]
  }
```

`inject.js`
```js
const script = document.createElement('script');
script.src = chrome.runtime.getURL('./assets/script.js');
document.head.appendChild(script);
```

`script.js`
```js
//Your source code for the addon
console.log("Hello World!");
```

`logo.png`
```
Use any logo for your extension. In this example 64x64 pixels are defined in the manifest file.
```

## Import extension in Chrome (or Chromium based browser)
1. Go to [chrome://extensions/](chrome://extensions/)
2. Enable Developer Mode
3. Click "Load unpacked" and select your extension folder

---

© unpacked - [licence](../../LICENSE)