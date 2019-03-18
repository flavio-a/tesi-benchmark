# tesi-benchmark
Benchmark set per la laurea triennale. Mi piacerebbe misurare sia i tempi di esecuzione che le righe di codice con i vari metodi.

# Confronti
Il benchmark set viene utilizzato per confrontare vari metodi per parallelismo in Haskell.

# Benchmarks
Elenco dei benchmark, in cui viene spiegato cosa testano e/o perché li ho scelti.

## Interessanti
Questi benchmark secondo me coprono quasi tutti i tipi di parallelismo comuni, manca solo un esempio di parallelismo dataflow (possibilmente con grafo dinamico).

- `Queens`: problema delle regine. Paradigma divide-et-impera. Possibile in due versioni: numero di soluzioni (divide-et-impera) o trovare una qualsiasi soluzione (parallelismo speculativo).
- `Minimax`: ricerca alpha-beta su un albero per un gioco a due giocatori. Paradigma divide-et-impera con pruning di rami inutili (con lazyness? GC? Esplicito? Immagino dipenderà dal tipo di parallelismo).
- `Transclos`: calcola tutti gli elementi raggiungibili da un insieme iniziale tramite una relazione data. Calcola in parallelo una lista infinita con data parallelism o producer-consumer.
- `Matmult`: moltiplicazione di matrici (algoritmo O(N^3) ingenuo). Data parallelism con clustering implicito.
- `Nbody`: problema degli n corpi, algoritmo di Barnes-Hut. Data parallelism su una struttura non lineare (quadtree).
- `Coins`: conta il numero di modi per realizzare una certa somma con monete di certi tipi. Paradigma divide-et-impera con soglia (clustering esplicito).

- Uno tra `sphere`, `ray`: raytracing. Usano nested data parallelism. Direi `sphere` perché il codice mi sembra organizzato meglio.

# Scartati
Benchmark scartati in quanto già sostituiti (a mio avviso) da altri.

- `Prsa`: parallel RSA encryption. Semplice esempio di data parallelism (parMap).
- `Linsolv`: risolutore di sistemi lineari interi tramite metodo modulare. Divide-et-impera.
- `Mandel`: insieme di Mandelbrot. Data parallelism.
- `Parfib`: calcolo dei numeri di Fibonacci con l'algoritmo esponenziale. Divide-et-impera.
- `Parfact`: calcolo del fattoriale. Clustering esplicito, divide-et-impera.
- `Sumeuler`: somma delle phi di Eulero di una lista di numeri. Nested data parallelism.
- `Hidden`: rimuove linee nascoste in un rendering 3D. Data parallelism.
-  `Gray`: per il parallelismo è come `ray` e `sphere`, ma il programma è molto più complesso (utile in casi reali ma non per il benchmark).

## Non trovati
Benchmark utilizzati in alcuni paper ma che non ho trovato, e che ritengo vengano adeguatamente sostituiti da altri.

- `Genetic`: allinea sequenze di DNA, usando divide-et-impera e nested data parallelism.
- `Maze`: esplorazione di un labirinto 2D. Parallelismo speculativo. Sostituito da `quuens` nella versione di trovare una soluzione (è lo stesso algoritmo).

## Altre note
Tutti i miei benchmark vengono da [nofib](https://gitlab.haskell.org/ghc/nofib). Ho cercato anche altri benchmark suites, ma tutte le altre trovate (es: haskell-CnC di Intel) contenevano molti benchmark in comune con nofib, e non ho trovato rilevanti quelli non in comune.
