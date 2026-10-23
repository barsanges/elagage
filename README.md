# Elagage

Pour un *livre dont vous êtes le héros* en cours de résolution, cet
outil permet d'identifier les branches déjà explorées qui ne
permettent pas de gagner.

## Mermaid

Le programme produit une chaîne de caractères exploitable par
[Mermaid](https://mermaid.js.org/). En envoyant le résultat vers
[mermaid-cli](https://github.com/mermaid-js/mermaid-cli), on peut donc
facilement produire un diagramme PDF, par exemple avec une commande du
type :

```bash
elagage -i input.json | mmdc -i - -o output.pdf --pdfFit
```

Sous Ubuntu, mermaid-cli peut notamment être installé via snap.

## Format des fichiers d'entrée

Le programme manipule un fichier décrivant (potentiellement de manière
partielle) la structure d'un livre. Il doit s'agir d'un fichier JSON
contenant un dictionnaire dont les clefs sont des numéros de
paragraphe et les valeurs des objets ayant un champ optionnel,
`"correspondances"`. Celui-ci contient la liste des paragraphes
directement accessibles depuis ce paragraphe.

Les numéros de paragraphe sont toujours représentés comme des chaînes
de caractères (e.g. `"123"` et non `123`).

Un exemple de fichier est donc :

```json
{
    "1":
    {
		"correspondances": ["2", "8"]
    },
    "8":
    {
		"correspondances": ["4"]
    }
}
```
