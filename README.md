# Haskell Sudoku Solver

Ein Sudoku-Solver implementiert in Haskell, der den Backtracking-Algorithmus verwendet, um Sudoku-Rätsel zu lösen.

## Features

- Löst 9x9 Sudoku-Rätsel
- Verwendet Backtracking-Algorithmus
- Visualisiert das Rätsel und die Lösung in der Konsole
- Überprüft die Gültigkeit von Zügen

## Eingabeformat

Sudokus werden als Liste von Integers repräsentiert, wobei:
- `-1` eine leere Zelle darstellt
- `1-9` vorgegebene Zahlen sind
- Die Liste wird zeilenweise von links nach rechts und von oben nach unten gelesen

Beispiel:
```haskell
sudokuList = [5,1,8,6,-1,-1,4,-1,-1,
              -1,-1,6,-1,-1,-1,-1,-1,7,
              4,3,-1,2,-1,-1,-1,-1,6,
              7,-1,-1,-1,-1,-1,-1,3,-1,
              -1,6,4,-1,1,8,2,-1,5,
              -1,5,-1,7,2,6,9,4,8,
              -1,-1,9,-1,-1,1,8,2,3,
              -1,2,1,-1,-1,-1,7,-1,-1,
              3,-1,-1,-1,4,-1,-1,6,9]
```

Dies repräsentiert folgendes Sudoku:
```
-------------------------
| 5 1 8 | 6     | 4     |
|     6 |       |     7 |
| 4 3   | 2     |     6 |
-------------------------
| 7     |       |   3   |
|   6 4 |   1 8 | 2   5 |
|   5   | 7 2 6 | 9 4 8 |
-------------------------
|     9 |     1 | 8 2 3 |
|   2 1 |       | 7     |
| 3     |   4   |   6 9 |
-------------------------
```

## Projektstruktur

## Implementierungsdetails

Der Solver verwendet einen Backtracking-Algorithmus mit folgenden Hauptschritten:
1. Finde eine leere Zelle
2. Versuche Zahlen von 1-9 einzusetzen
3. Prüfe ob der Zug legal ist
4. Wenn legal, gehe rekursiv zum nächsten leeren Feld
5. Wenn keine Zahl passt, gehe zurück (Backtracking) und versuche die nächste Zahl

## To-Do

- [ ] Implementierung eines Sudoku-Generators
- [ ] Löschen von matrix.hs, wenn Projekt fertig
- [ ] Projektstruktur anpassen
- [ ] .exe erstellen