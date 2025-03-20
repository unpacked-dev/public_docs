# Deno CLI CSV splitter
This cli tool comes in handy if you got a csv file with too much lines to handle and would like to split it in multiple files.

# Usage
`deno run --allow-read --allow-write splitter.cli.js <input_file> <lines> [<output_folder>]`

# Source Code
`splitter.cli.js`

```js
if (Deno.args.length < 2) {
    console.log("Usage:");
    console.log("deno run --allow-read --allow-write splitter.cli.js <input_file> <lines> [<output_folder>]");
    console.log("Note: CSV cannot be split if it contains \\n within the contents");
    Deno.exit(1);
}

const [input_file, line_count_arg, output_folder_arg] = Deno.args;
const linesPerFile = parseInt(line_count_arg, 10);
if (isNaN(linesPerFile) || linesPerFile < 1) {
    console.error("The number of lines must be a positive number.");
    Deno.exit(1);
}
const outputFolder = output_folder_arg ? output_folder_arg : "output";

const file_content = await Deno.readTextFile(input_file);
const lines = file_content.split("\n");
if (lines.length < 2) {
    console.error("The file must contain at least a header and one data line.");
    Deno.exit(1);
}
const header_line = lines[0];

await Deno.mkdir(outputFolder, { recursive: true });

let fileIndex = 1;
for (let i = 1; i < lines.length; i += linesPerFile) {
    const chunk = lines.slice(i, i + linesPerFile);
    const content = [header_line, ...chunk].join("\n");
    await Deno.writeTextFile(`${outputFolder}/part_${fileIndex}.csv`, content);
    fileIndex++;
}
```

---
© unpacked - [licence](../../LICENSE)