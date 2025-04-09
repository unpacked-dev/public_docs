# Regex  Cheatsheet für JavaScript

## Einführung
Regex  (Regular Expressions) sind Muster, mit denen du Strings durchsuchen, matchen, validieren oder ersetzen kannst.

## Erstellen von Regex in JavaScript
### Literal-Syntax:
```js
const regex = /muster/;
```

### RegExp-Konstruktor:

```js
const regex = new RegExp("muster");
```

## Wichtige Methoden
```regex.test(string)``` 
Liefert true oder false, wenn der String zum Muster passt. <br>

```string.match(regex)```
Gibt ein Array mit den Treffer(n) zurück oder null. <br>

```string.replace(regex, ersatz)```
Ersetzt Treffer im String durch den ersatz-String. <br>

```string.search(regex)```
Liefert den Index des ersten Treffers oder -1. <br>

```string.split(regex)```
Teilt den String anhand des Musters in ein Array. <br>

## Wichtige Zeichen & Metazeichen
| Symbol | Bedeutung | Beispiel |
|-|-|-|
| `.` |	Beliebiges Zeichen außer Zeilenumbruch | `/h.llo/` matcht "hallo", "hxllo" |
| `*` |	0 oder mehr Wiederholungen | `/lo*/` matcht "l", "lo", "loo" |
| `+` |	1 oder mehr Wiederholungen | `/lo+/` matcht "lo", "loo", ... |
| `?` |	Optional (0 oder 1-mal) oder lazy Quantifier | `/colou?r/` matcht "color" und "colour" <br> `/lo+?/` (lazy) | 
| `^` |	Anfang des Strings | `/^Hi/` matcht "Hi..." |
| `$` |	Ende des Strings | `/Bye$/` matcht "...Bye" |
| `\d` | Ziffer (0-9) | `/\d\d/` matcht "42" |
| `\w` | Wortzeichen (a-z, A-Z, 0-9, _) | `/\w+/` matcht "hello_42" |
| `\s` | Whitespace-Zeichen (Leerzeichen, Tab, etc.) | `/\s/` matcht ein Leerzeichen |
| `[abc]` |	Zeichenklasse: a, b oder c | `/[hc]at/`matcht "hat" oder "cat" |
| `[^abc]` | Alle Zeichen außer a, b oder c	| `/[^0-9]/` matcht alles außer Ziffern |
| `\b` | Wortgrenze	| `/\bword\b/` findet "word" als ganzes Wort |
| `\B` | Keine Wortgrenze | `/\Bend\B/` findet "end" innerhalb von Wörtern |

## Gruppierung & Alternation
### Klammern (Capturing Group):
Gruppiert Teile des Musters, z.B. (abc)
Ermöglicht späteres Referenzieren (Backreferencing).

### Non-Capturing Group:
(?:...) gruppiert, ohne den Inhalt zu speichern.

### Alternation (ODER):
Das Pipe-Symbol | ermöglicht Alternativen, z.B. /foo|bar/ matcht "foo" oder "bar".

## Regex-Flags
Flags werden hinter dem Schrägstrich angegeben:

- `g` (global): Alle Treffer finden.
- `i` (ignore case): Groß-/Kleinschreibung ignorieren.
- `m` (multiline): ^ und $ berücksichtigen Zeilenumbrüche.
- `s` (dotAll): . matcht auch Zeilenumbrüche.
- `u` (Unicode): Unicode-Support aktivieren.
- `y` (sticky): Sucht direkt ab der letzten Position.

<br>

---
© unpacked - [licence](../../LICENSE)