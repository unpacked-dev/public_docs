# ABAP ERP/CRM Cheatsheet
A (very) small cheatsheet for SAP ERP/CRM knowledge. <br>
🚧 under construction and will be updated eventually 🚧

## Transactions

### Prefix
`/n` New transction  <br>
`/o` New window  <br>

### Suffix
`se01` Transports <br>
`se16` Tables <br>
`se37` Function modules  <br>
`se38` Reports <br>
`sm30` (Table) Customizing <br>
`sm59` RFC-Destinations <br>
`otr_edit` OTR-Texts <br>
`st22` (Short) Dumps <br>
`stms` Transport management system <br>
`sproxy` Proxylass for webservice calls <br>
`/IWFND/GW_CLIENT` SAP Gateway Client (if not exist try report below) <br>
`/IWFND/ERROR_LOG` SAP Gateway Error Log (if not exist try report below) <br>
`/IWFND/MAINT_SERVICE` Activate and Maintain Gateway Services (if not exist try report below) <br>
`/IWBEP/SB` SAP Gateway Service Builder <br>

---

## Tables

`T005` Languages <br>
`T005T` Countries <br>
`TSAD3T` Salutations <br>
`T173T` Shipping Types <br>
`KNA1` Debitor information <br>
`KNVK` Contact person information <br>
`BUT000` CRM Business Partner <br>
`BUT100` CRM Business Partner Roles <br>

---

## Reports
`RS_ABAP_SOURCE_SCAN` Sourcecode scan <br>
`/IWFND/SUTIL_GW_CLIENT` SAP Gateway Client <br>
`/IWFND/SUTIL_LOG` SAP Gateway Error Log <br>
`/IWFND/R_MGW_REGISTRATION` Activate and Maintain Gateway Services <br>
`/IWBEP/R_SBUI_SERVICE_BUILDER` SAP Gateway Service Builder <br>

---

## Function modules
`BAPI_BUPA_CENTRAL_GETDETAIL` Central data of BP <br>
`BAPI_BUPA_ADDRESS_GETDETAIL` Adress data of BP <br>

---

## SAP GUI

### Pretty Printer Config
`Utilities (Hilfsmittel) -> Settings (Einstellungen)`
```
Indent (Einrücken) = X
Convert Uppercase/Lowercase (Groß-/Kleinkonvertierung)= X
    Uppercase Keyword (Schlüsselwort groß) = X
```

### Change Object Directory Entries
Change Packet, Original System, Person Responsible etc. for an Object

1. Transaction `se03`
2. `Object Directory -> Change Object Directory Entries`
3. Select by Object (Class, Programm, FM etc.)
4. Execute `F8`
5. Edit `F1`

---
© unpacked - [licence](../../LICENSE)
