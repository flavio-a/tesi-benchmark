# tesi-benchmark
Benchmark set per la laurea triennale. Mi piacerebbe misurare sia i tempi di esecuzione che le righe di codice con i vari metodi.

# Confronti
Il benchmark set viene utilizzato per confrontare vari metodi per parallelismo in Haskell. I metodi studiati sono i seguenti:
- implicito. Sembra interessante FDIP, ma non penso di testarlo (però penso di usare risultati di altri come paragone).
- (flat) data parallelism. Implementato in [Repa](http://repa.ouroborus.net/). Sembrava più interessante Data Parallel Haskell dato che permetteva nested data parallelism, ma il progetto [è stato interrotto nel 2010](https://gitlab.haskell.org/ghc/ghc/wikis/data-parallel).
- con strategie. Implementato nella libreria [Control.Parallel](http://hackage.haskell.org/package/parallel) di Haskell.
- dataflow. Implementato nella libreria [Control.Monad.Par](http://hackage.haskell.org/package/monad-par) di Haskell.

Altri framework:
- [Eden](http://www.mathematik.uni-marburg.de/~eden/): parallelismo con thread espliciti, in qualche modo simile alle strategie, ma parallelizza a livello di funzioni invece che di thunk da valutare.
- [Accelerate](https://www.acceleratehs.org/get-started.html): flat data parallelism, molto simile a Repa.
- [haskell-cnc](http://hackage.haskell.org/package/haskell-cnc): parallelismo dataflow, simile a Control.Monad.Par ma più avanzata.

# Benchmarks
Elenco dei benchmark, in cui viene spiegato cosa testano e/o perché li ho scelti.

## Interessanti
Questi benchmark secondo me coprono quasi tutti i tipi di parallelismo comuni, manca solo un esempio di parallelismo dataflow (possibilmente con grafo dinamico).

- `Queens`: problema delle regine. Paradigma divide-et-impera. Possibile in due versioni: numero di soluzioni (divide-et-impera) o trovare una qualsiasi soluzione (parallelismo speculativo, inutile se trovo il gate array simulator).
- `Minimax`: ricerca alpha-beta su un albero per un gioco a due giocatori. Paradigma divide-et-impera con pruning di rami inutili (con lazyness? GC? Esplicito? Immagino dipenderà dal tipo di parallelismo).
- `Transclos`: calcola tutti gli elementi raggiungibili da un insieme iniziale tramite una relazione data. Calcola in parallelo una lista infinita con data parallelism o producer-consumer.
- `Matmult`: moltiplicazione di matrici (algoritmo O(N^3) ingenuo). Data parallelism con clustering implicito.
- `Nbody`: problema degli n corpi, algoritmo di Barnes-Hut. Nested data parallelism su una struttura non lineare (quadtree).
- `Coins`: conta il numero di modi per realizzare una certa somma con monete di certi tipi. Paradigma divide-et-impera con soglia (clustering esplicito).

- Uno tra `sphere`, `ray`: raytracing. Usano nested data parallelism. Direi `sphere` perché il codice mi sembra organizzato meglio.

- Un gate array simulator. Si presta bene a parallelismo speculativo e dataflow. `Circsim` in nofib/spectral fa molto più del necessario e probabilmente fa troppo, nel senso che non si riesce a parallelizzare come vorrei.

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
