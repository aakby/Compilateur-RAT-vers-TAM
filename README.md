# Compilateur-RAT-vers-TAM

Le but de ce projet de programmation fonctionnelle et de traduction des langages est de créer un
compilateur du langage type RAT qui traduit un code en language RAT en un code en language TAM proche du language assembleur.
On cherche à générer un code TAM qui, si on l’exécute, a le même effet que celui défini par le code source.

Grammaire formelle du langage Rat :

1. PROG' → PROG
2. PROG → FUN PROG
3. FUN → TYPE id ( DP ) {IS return E ; }
4. P ROG → id BLOC
5. BLOC → { IS }
6. IS → I IS1
7. IS →
8. I → TYPE id = E ;
9. I → id = E ;
10. I → const id = entier ;
11. I → print E;
12. I → if E BLOC1 else BLOC2
13. I → while E BLOC
14. DP →
15. DP → T Y P E id DP
16. T Y P E → bool
17. T Y P E → int
18. T Y P E → rat
19. E → call id ( CP )
20. CP →
21. CP → E CP
22. E → [ E / E ]
23. E → num E
24. E → denom E
25. E → id
26. E → true
27. E → f alse
28. E → entier
29. E → ( E + E )
30. E → ( E ∗ E )
31. E → ( E = E )
32. E → ( E < E )

Le projet est fait en suivant les étapes suivantes :

  - Analyses lexicale et syntaxique et génération de l’arbre abstrait

  - Analyse sémantique : Réalisation du compilateur par passes :
                       
             -  Passe de résolution des identifiants
             -  Passe de typage
             -  Passe de placement mémoire
             -  Passe de génération du code 


Enfin, on a un code en language TAM (proche du language assembleur) compréhensible par l'ordinateur.
