# Deno KV CRUD example using CLI

Create, Update, Read and Delete Values from local Deno KV database.
```js
if (Deno.args.length === 0) {
  console.log("Usage:");
  console.log("deno run --unstable-kv --allow-net cli-kv.ts <operation> <key> [<value>]");
  console.log("Operations: create, read, update, delete");
  Deno.exit(1);
}

const [operation, key, value] = Deno.args;

const kv = await Deno.openKv();

switch (operation.toLowerCase()) {
  case "create":
  case "update":
    if (!key || !value) {
      console.error("Error: For 'create' or 'update', both key and value must be provided.");
      Deno.exit(1);
    }
    await kv.set(["kv", key], { value });
    console.log(`Key "${key}" has been set to value "${value}".`);
    break;

  case "read":
    if (!key) {
      console.error("Error: For 'read', a key must be provided.");
      Deno.exit(1);
    }
    const result = await kv.get(["kv", key]);
    if (result.value) {
      console.log(`Key "${key}" has value: ${result.value.value}`);
    } else {
      console.log(`No entry found for key "${key}".`);
    }
    break;

  case "delete":
    if (!key) {
      console.error("Error: For 'delete', a key must be provided.");
      Deno.exit(1);
    }
    await kv.delete(["kv", key]);
    console.log(`Key "${key}" has been deleted.`);
    break;

  default:
    console.error("Unknown operation. Please use: create, read, update, or delete.");
    Deno.exit(1);
}
```

Run the following example command: <br>
`deno run --unstable-kv --allow-net cli-kv.ts <operation> <key> [<value>]`