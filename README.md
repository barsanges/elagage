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
elagage -i input.json | mmdc -i - -o output.pdf
```

Sous Ubuntu, mermaid-cli peut notamment être installé via snap.

## Format des fichiers d'entrée

Le programme manipule un fichier décrivant (potentiellement de manière
partielle) la structure d'un livre. Il doit s'agir d'un fichier JSON
contenant un dictionnaire dont les clefs sont des numéros de
paragraphe et les valeurs des objets ayant 3 champs, tous optionnels :

* `"correspondances"`, la liste des paragraphes directement
  accessibles depuis ce paragraphe.

* `"cul de sac"`, un élément qui indique sous quelles conditions le
  paragraphe est un cul de sac (c'est-à-dire qu'il ne permet pas
  d'aller plus loin et constitue donc une défaite). Ce champ est
  optionnel ; s'il est absent, on considère que le paragraphe n'est
  jamais un cul-de-sac. `"cul de sac"` peut être soit un booléen, soit
  un objet :

	* si c'est un booléen, `true` indique que le paragraphe est
      toujours un cul de sac, et `false` que le paragraphe n'est
      jamais un cul de sac ;

	* si c'est un objet, il doit avoir un champ `"noeuds
      inhibiteurs"`, qui contient une liste de numéros de paragraphes,
      et un champ `"noeuds déclencheurs"`, qui contient une liste de
      liste de numéros de paragraphes. Le paragraphe est alors un cul
      de sac si et seulement si le joueur est passé par au moins un
      paragraphe de chaque liste de `"noeuds déclencheurs"`, et par
      aucun paragraphe de la liste de `"noeuds inhibiteurs"`. Si un
      paragraphe est décrit à la fois comme déclencheur et inhibiteur,
      il est considéré comme inhibiteur.

* `"fin"`, un objet qui indique sous quelles conditions le paragraphe
  est un paragraphe de fin victorieuse. Ce champ est optionnel ; s'il
  est absent, on considère que le paragraphe n'est jamais un
  paragraphe de fin. `"fin"` peut être soit un booléen, soit un objet
  :

	* si c'est un booléen, `true` indique que le paragraphe correspond
      toujours à une fin victorieuse, et `false` que le paragraphe
      n'est jamais un paragraphe de fin victorieuse ;

	* si c'est un objet, il doit avoir un champ `"noeuds
      inhibiteurs"`, qui contient une liste de numéros de paragraphes,
      et un champ `"noeuds déclencheurs"`, qui contient une liste de
      liste de numéros de paragraphes. Le paragraphe est alors un
      paragraphe de fin victorieuse si et seulement si le joueur est
      passé par au moins un paragraphe de chaque liste de `"noeuds
      déclencheurs"`, et par aucun paragraphe de la liste de `"noeuds
      inhibiteurs"`. Si un paragraphe est décrit à la fois comme
      déclencheur et inhibiteur, il est considéré comme inhibiteur.

Les numéros de paragraphe sont toujours représentés comme des chaînes
de caractères (e.g. `"123"` et non `123`).

Un exemple de fichier est donc :

```json
{
    "1":
    {
		"correspondances": ["2", "8"],
		"cul de sac": false,
		"fin": false
    },
    "8":
    {
		"correspondances": ["4"],
		"cul de sac":
		{
			"noeuds inhibiteurs": ["12", "14", "5"],
			"noeuds déclencheurs":
			[
				["17", "6", "9"]
			]
		},
		"fin": false
    }
}
```
