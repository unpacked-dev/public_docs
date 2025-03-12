# Await oModel data load
When calling `oModel.getData()` it's possible that the data has not been loaded yet and so returns undefined.
With this little code snippet you can await the model load and make sure the data has been loaded in.

`Page.controller.js`
```js
const oModel = new JSONModel();
await this.loadModelData(oModel, "PATH");
const modelData = oModel.getData();

//Helper function to return promise
loadModelData: function (oModel, sUrl) {
    return new Promise((resolve, reject) => {
        oModel.attachRequestCompleted(() => {
            resolve(oModel.getData());
        });
        oModel.attachRequestFailed((oEvent) => {
            reject(new Error(oEvent.getParameter("message")));
        });
        oModel.loadData(sap.ui.require.toUrl(sUrl));
    });
}
```

---
© unpacked - [licence](../../LICENSE)