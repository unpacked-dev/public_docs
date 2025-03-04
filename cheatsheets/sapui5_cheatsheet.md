# SAP UI5 Blueprint
A blueprint for a simple sap ui5 app.

## Getting started
1. Download [SAP UI5 Runtime](https://tools.hana.ondemand.com/#sapui5)
2. Create a new folder for your project e.g. `ui5_example`
3. Unzip Runtime into your project folder
4. Create the directory structure in your project folder

```
ui5_example/
├── index.html
├── Component.js
├── manifest.json
├── i18n/
│   └── i18n.properties
├── view/
│   ├── App.view.xml
│   ├── Main.view.xml
│   └── Page1.view.xml
├── controller/
│   ├── App.controller.js
│   ├── Main.controller.js
│   └── Page1.controller.js
├── resources/... (ui5 runtime)
└── discovery/... (ui5 runtime)
```

5. File setup [see below](#file-setup)
6. Open `index.html` in your Browser

## File setup
🚩 If you named your project anything other than `ui5_example` you have to replace `ui5_example` in all files with your project folder name.

### index.html
The entry point where the UI5 Bootstrap is loaded.

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Meine UI5 App</title>
    <script src="./resources/sap-ui-core.js"
      id="sap-ui-bootstrap"
      data-sap-ui-libs="sap.m"
      data-sap-ui-theme="sap_belize_dark"
      data-sap-ui-resourceroots='{"ui5_example": "./"}'
      data-sap-ui-xx-componentPreload="off"
      data-sap-ui-oninit="module:sap/ui/core/ComponentSupport"
      data-sap-ui-preload="async">
    </script>
    <style>
      html, body { height: 100%; }
    </style>
  </head>
  <body class="sapUiBody" id="content">
    <div
      data-sap-ui-component
      data-name="ui5_example"
      data-id="ui5_example"
      style="height: 100%"
      data-settings='{"id":"ui5_example"}'>
    </div>
  </body>
</html>
```

### Component.js
The central component that, among other things, references the manifest.

```js
sap.ui.define([
  "sap/ui/core/UIComponent"
], function(UIComponent) {
  "use strict";
  return UIComponent.extend("ui5_example.Component", {
    metadata: {
      manifest: "json"
    },
    init: function() {
      UIComponent.prototype.init.apply(this, arguments);
      this.getRouter().initialize();
    }
  });
});
```

### manifest.json
The configuration file for your app (e.g., routing, dependencies, app data).

```json
{
    "sap.app": {
      "id": "ui5_example",
      "type": "application",
      "i18n": "i18n/i18n.properties",
      "title": "Meine UI5 App",
      "description": "Minimal Beispiel"
    },
    "sap.ui": {
      "technology": "UI5"
    },
    "sap.ui5": {
      "rootView": {
        "viewName": "ui5_example.view.App",
        "type": "XML",
        "id": "app"
      },
      "dependencies": {
        "minUI5Version": "1.60",
        "libs": {
          "sap.m": {}
        }
      },
      "routing": {
        "config": {
          "routerClass": "sap.m.routing.Router",
          "viewType": "XML",
          "viewPath": "ui5_example.view",
          "controlId": "app",
          "controlAggregation": "pages",
          "bypassed": {
            "target": "notFound"
          }
        },
        "routes": [
          {
            "pattern": "",
            "name": "main",
            "target": "main"
          },
          {
            "pattern": "page1",
            "name": "page1",
            "target": "page1"
          }
        ],
        "targets": {
          "main": {
            "viewName": "Main",
            "viewLevel": 1
          },
          "page1": {
            "viewName": "Page1",
            "viewLevel": 2
          }
        }
      }
    }
  }
```

### i18n.properties
Translation files

```properties
appTitle=Meine UI5 App
```

### App.view.xml
XML views for the individual pages

```xml
<mvc:View
  xmlns:mvc="sap.ui.core.mvc"
  xmlns="sap.m"
  controllerName="ui5_example.controller.App">
  <App id="app" />
</mvc:View>
```

### Main.view.xml
```xml
<mvc:View
	xmlns="sap.m"
	xmlns:mvc="sap.ui.core.mvc"
	xmlns:core="sap.ui.core"
	height="100%"
	controllerName="ui5_example.controller.Main">
	<Page
		title="Page">
		<content>
			<Button type="Emphasized" 
                text="Navigate to Page 1" 
                press="onNavToPage1" 
            />
		</content>
	</Page>
</mvc:View>
```

### Page1.view.xml
```xml
<mvc:View
  xmlns="sap.m"
	xmlns:mvc="sap.ui.core.mvc"
	xmlns:core="sap.ui.core"
	height="100%"
  controllerName="ui5_example.controller.Page1">
  <Page title="Page 1">
    <content>
        <Button 
            text="Back" 
            press="onNavBack"
        />
    </content>
  </Page>
</mvc:View>
```

### App.controller.js
Controllers that define the behavior of the views

```js
sap.ui.define([
    "sap/ui/core/mvc/Controller"
  ], function(Controller) {
    "use strict";
    return Controller.extend("ui5_example.controller.App", {
      
    });
});
```

### Main.controller.js

```js
sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function(Controller) {
  "use strict";
  return Controller.extend("ui5_example.controller.Main", {
    onNavToPage1: function() {
      console.log("Nav")
      var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
      oRouter.navTo("page1");
    }
  });
});
```

### Page1.controller.js

```js
sap.ui.define([
    "sap/ui/core/mvc/Controller"
  ], function(Controller) {
    "use strict";
    return Controller.extend("ui5_example.controller.Page1", {
      onNavBack: function() {
        var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
        oRouter.navTo("main");
      }
    });
});  
```