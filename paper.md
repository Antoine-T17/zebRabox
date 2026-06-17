---
title: "zebRabox: An R/Shiny Application for Importing, Processing, and Visualizing ZebraBox Behavioural Tracking Data"
authors:
  - name: Antoine Tourret
    orcid: 0009-0004-0895-8368
    corresponding: true
    affiliation: 1
affiliations:
  - name: Laboratoire Eau Environnement et Systèmes Urbains
    index: 1
    ror: https://ror.org/02nqy7n35
date: 27 May 2026
bibliography: paper.bib
---

<!-- ======================================================================
     INSTRUCTIONS JOSS
     - Longueur cible : 750–1 750 mots (corps du texte hors YAML et refs)
     - Toutes les sections ci-dessous sont OBLIGATOIRES
     - Les balises HTML <!-- --> sont des commentaires de travail à supprimer
       avant soumission
     - Compléter paper.bib avec les références BibLaTeX correspondantes
     ====================================================================== -->

# Summary

<!-- Objectif : expliquer la fonctionnalité principale du logiciel à un
     lecteur non-spécialiste. Répondre à "que fait ce logiciel ?".
     Longueur suggérée : ~150–200 mots. -->

<!-- À rédiger :
     - Présenter ZebraBox (ViewPoint Behaviour Technology) en une phrase :
       dispositif de suivi comportemental d'organismes aquatiques (poissons-zèbres,
       daphnies…) utilisé en écotoxicologie et neurosciences.
     - Décrire le problème : les fichiers bruts exportés (.xls/.xlsx et .zip
       Wintrack) requièrent un pipeline manuel fastidieux (nettoyage, assignation
       des périodes lumière/obscurité ou vibration, normalisation par plaque) avant
       toute analyse statistique ou visualisation.
     - Introduire zebRabox : package R wrappant une application Shiny guidant
       l'utilisateur en quatre étapes (Plan de plaque → Données brutes →
       Traitement → Visualisation) via une interface graphique sans écriture de code.
     - Mentionner les formats d'entrée supportés, les modes analytiques (Tracking /
       Quantization × Light-Dark / Vibration), et les sorties (figures interactives
       ggiraph, exports Excel, scripts R reproductibles).
-->

[À rédiger]

---

# Statement of Need

<!-- Objectif : justifier l'existence du logiciel dans le contexte de la
     recherche. Répondre à "pourquoi ce logiciel était-il nécessaire ?".
     Longueur suggérée : ~200–250 mots. -->
     
     

## 1.1 Contexte scientifique

<!-- À rédiger :
     - Décrire l'usage du ZebraBox en recherche (écotoxicologie, pharmacologie,
       neurosciences du comportement).
     - Souligner le volume de données généré par les expériences multi-plaques
       et la diversité des modes d'acquisition.
-->

[À rédiger]

## 1.2 Limites des approches actuelles

<!-- À rédiger :
     - Absence d'outil open-source intégré pour ce type de données.
     - Les utilisateurs actuels recourent à des scripts ad hoc (R, Python, Excel),
       non reproductibles et non documentés.
     - Mentionner les erreurs fréquentes : mauvais alignement plaque/plan,
       erreurs de découpage temporel (périodes), pertes de données.
-->

[À rédiger]

## 1.3 Public cible

<!-- À rédiger :
     - Chercheurs et ingénieurs d'étude en biologie comportementale sans
       compétences avancées en programmation.
     - Laboratoires équipés du ZebraBox (ViewPoint) souhaitant standardiser
       leur pipeline d'analyse.
-->

[À rédiger]

---

# State of the Field

<!-- Objectif : comparer zebRabox aux outils existants. Montrer la
     complémentarité ou la supériorité sur des points précis.
     Longueur suggérée : ~200–250 mots. -->

## 2.1 Outils de visualisation comportementale existants

<!-- À rédiger :
     - Ethovision XT (Noldus) : logiciel propriétaire, coûteux, généraliste.
     - DanioVision (Noldus) : dédié poisson-zèbre mais propriétaire.
     - viewr (R) [@Bhatt2022] : package R pour trajectoires mais pas spécifique ZebraBox.
     - idtracker.ai : tracking par IA, pas de pipeline ZebraBox intégré.
     - Logiciels propriétaires ViewPoint (Videotrack) : exportent des données
       brutes mais ne fournissent pas d'analyse statistique intégrée.
-->

[À rédiger]

## 2.2 Positionnement de zebRabox

<!-- À rédiger :
     - Seul outil open-source dédié au format ZebraBox (.xlsx + .zip Wintrack).
     - Interface Shiny : aucune compétence en programmation requise.
     - Pipeline reproductible : export de scripts R générés automatiquement.
     - Statistiques intégrées (tests paramétriques/non-paramétriques, post-hoc,
       diagnostics interactifs).
-->

[À rédiger]

## 2.3 Références bibliographiques clés à citer

<!-- À intégrer dans paper.bib :
     - Articles décrivant l'utilisation du ZebraBox en recherche.
     - Articles de méthodes sur l'analyse comportementale automatisée.
     - Packages R pertinents (shiny, ggiraph, ggplot2, DT…).
     - Éventuels articles issus du propre laboratoire de l'auteur utilisant
       le ZebraBox.
-->

[À rédiger]

---

# Software Design

<!-- Objectif : décrire les choix architecturaux et techniques.
     Longueur suggérée : ~300–400 mots. -->

## 3.1 Architecture générale

<!-- À rédiger :
     - Package R standard (structure CRAN-compatible) wrappant une application
       Shiny dans inst/app/.
     - Point d'entrée unique : zebRabox::run_app().
     - Quatre onglets correspondant aux quatre étapes du pipeline :
       Plan de plaque → Données brutes → Traitement → Visualisation.
     - Communication inter-modules via un objet reactiveValues (rv) centralisé
       dans server.R : pas de couplage direct entre modules.
-->

[À rédiger]

## 3.2 Système de modes analytiques

<!-- À rédiger :
     - Quatre modes définis par une clé composite :
       tm_ldm (Tracking + Light-Dark), tm_vm (Tracking + Vibration),
       qm_ldm (Quantization + Light-Dark), qm_vm (Quantization + Vibration).
     - Factories get_processing_config() et get_visualization_config()
       retournant des listes de paramètres spécifiques au mode.
     - Instanciation paresseuse (lazy) des modules Processing et Visualization :
       évite les erreurs de contexte réactif Shiny.
-->

[À rédiger]

## 3.3 Pipeline de traitement

<!-- À rédiger :
     - Étapes séquentielles : (1) filtrage optionnel (mode Quantization),
       (2) génération des conditions (jointure plan de plaque), (3) assignation
       des périodes (fichier de transitions temporelles), (4) suppressions
       paramétrables (codes-temps, puits, conditions, périodes),
       (5) conversion numérique, (6) décomposition par zones (process_zones).
     - Gestion des fichiers ZIP (trajectoires Wintrack .txt) : pipeline parallèle
       avec correspondance XLSX↔ZIP par préfixe de 6 caractères du champ location.
-->

[À rédiger]

## 3.4 Visualisation interactive

<!-- À rédiger :
     - Figures produites avec ggplot2 et rendues interactives via ggiraph
       (tooltips, sélection).
     - Quatre types de figures : boxplots par période, boxplots cumulatifs,
       boxplots delta, lineplots temporels.
     - Figures de diagnostic statistique (QQ-plot, résidus vs. fitted, leverage,
       boxplot by condition) également converties en ggiraph.
     - Thèmes light/dark commutables.
     - Export : figures (.svg/.png), datasets (.xlsx, par variable ou toutes
       variables), scripts R reproductibles générés automatiquement.
-->

[À rédiger]

## 3.5 Choix techniques justifiés

<!-- À rédiger :
     - data.table pour les opérations sur grands dataframes (build_lineplot_df,
       prepare_all_zone) : performance sur jeux de données multi-plaques.
     - ggiraph plutôt que plotly : intégration native ggplot2, tooltips HTML
       personnalisables, poids plus léger côté client.
     - shinydashboard + fresh : thème personnalisable sans CSS custom lourd.
     - openxlsx pour l'export multi-feuilles : un onglet Excel par variable.
-->

[À rédiger]

---

# Research Impact Statement

<!-- Objectif : démontrer que le logiciel a un impact réel ou imminent
     sur la recherche. C'est un critère éliminatoire JOSS.
     Longueur suggérée : ~150–200 mots. -->

## 4.1 Utilisation dans des travaux de recherche

<!-- À rédiger :
     - Citer les expériences ou publications (en cours ou publiées) du
       laboratoire de l'auteur ayant utilisé ce pipeline.
     - Si données déjà collectées avec le ZebraBox et analysées avec une
       version antérieure du package : le mentionner explicitement.
-->

[À rédiger]

## 4.2 Potentiel d'adoption

<!-- À rédiger :
     - Nombre de laboratoires équipés du ZebraBox dans le monde
       (si disponible dans la littérature).
     - Communauté R/Shiny : facilité d'installation (install_github),
       aucun prérequis logiciel propriétaire.
     - Perspective de citation dans des études écotoxicologiques ou
       pharmacologiques utilisant le ZebraBox.
-->

[À rédiger]

## 4.3 Reproductibilité et standardisation

<!-- À rédiger :
     - La génération automatique de scripts R reproductibles permet la
       traçabilité complète du pipeline (FAIR principles).
     - Les exports structurés (.xlsx multi-feuilles) facilitent la
       réutilisation des données dans d'autres environnements.
-->

[À rédiger]

---

# AI Usage Disclosure

<!-- OBLIGATOIRE selon les nouvelles règles JOSS.
     Décrire précisément les outils IA utilisés, leur rôle, et confirmer
     que l'auteur humain a supervisé et validé toutes les sorties. -->

Ce logiciel a été développé avec l'assistance de Claude (Anthropic,
modèles Sonnet 4.x), utilisé via l'interface Claude Code (CLI) pour :

- la génération et la refactorisation de code R/Shiny,
- la structuration de l'architecture modulaire de l'application,
- la rédaction de commentaires et de documentation inline.

L'ensemble des sorties générées par l'IA ont été relues, vérifiées
et modifiées le cas échéant par l'auteur. Les décisions de conception
(choix des modes analytiques, structure du pipeline, sélection des
bibliothèques) ont été prises par l'auteur. Claude Code n'a pas été
utilisé pour la rédaction du présent article.

[À adapter selon l'utilisation effective au moment de la soumission]

---

# Acknowledgements

<!-- Remercier les contributeurs, financeurs, institutions.
     Section courte (~50–100 mots). -->

<!-- À rédiger :
     - Remercier le directeur/directrice de thèse.
     - Mentionner l'institution d'accueil et le financement doctoral
       (bourse, contrat…).
     - Remercier éventuellement les collègues ayant testé l'application
       ou fourni des jeux de données.
     - Mentionner ViewPoint Behaviour Technology si les données d'exemple
       proviennent de leur dispositif (sans en faire une promotion).
-->

[À rédiger]

---

# References

<!-- Les références sont générées automatiquement depuis paper.bib
     lors de la compilation Pandoc par JOSS.
     Ne pas lister manuellement ici — tout passe par paper.bib. -->
