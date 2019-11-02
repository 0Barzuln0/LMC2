# LMC2
## Little Man Computer (versione in Common Lisp)

Questo è lo svilupppo del progetto **Little Man Computer** per l'esame '*Linguaggi di Programmazione*' del corso di laurea triennale in Informatica dell'Università di Milano-Bicocca.

Il progetto è stato concluso da due persone in circa due mesi, comprendendo lo sviluppo sia della versione Common Lisp che della versione [Prolog](https://github.com/0Barzuln0/LMC "https://github.com/0Barzuln0/LMC").

## Overview del progetto
Il little man computer (LMC) è un semplice modello di computer creato per scopi didattici.  
Esso possiede 100 celle di memoria (numerate da 0 a 99) ognuna della quali può contenere un
numero da 0 a 999 (estremi inclusi). Il computer possiede un solo registro, detto accumulatore,
una coda di input ed una coda di output. LMC possiede un numero limitato di tipi di istruzioni
ed un equivalente assembly altrettanto semplificato.
  
Lo scopo del progetto è quello di produrre:
-  Un simulatore del LMC che dato il contenuto iniziale delle memoria (una lista di 100 numeri)
e una sequenza di valori di input simuli il comportamento del LMC e produca il contenuto
della coda di output dopo l’arresto del LMC.
-  Un assembler che, dato un file scritto nell’assembly semplificato del LMC produca il
contenuto iniziale della memoria.
