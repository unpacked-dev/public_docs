# Deno Progressbar
A very minimalistic progressbar for Deno terminal output.

```js
function progressbar(current_value, total_value, additional_text = "") {
    const encoder = new TextEncoder();
    const bars  = Math.floor(current_value / (total_value / 20));
    const line = `\r[${"#".repeat(bars)}${"-".repeat(20 - bars)}]`;
    const percent = `${Math.floor((current_value / total_value) * 100)}%`;

    Deno.stdout.writeSync(encoder.encode(`${line} ${percent} ${additional_text ? `| ${additional_text}` : ""}`));
}
```

# 🚀 Usage
Just copy the code above to your project and call the progressbar function. <br> 
```js
progressbar(25, 100, "Loading...");
```
```
[#####---------------] 25% | Loading...
```

Make sure to print a new line with `\n` when the progressbar is done to continue writing text.

---
© unpacked - [licence](../../LICENSE)