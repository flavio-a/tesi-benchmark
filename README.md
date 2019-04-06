# tesi-benchmark
Benchmark suite per la laurea triennale. Mi piacerebbe misurare sia i tempi di esecuzione che le righe di codice/impatto linguistico dei vari metodi.

# Confronti
Il benchmark suite viene utilizzato per confrontare vari metodi per parallelismo in Haskell. I metodi studiati sono i seguenti:
- (flat) data parallelism. Implementato in [Repa](http://repa.ouroborus.net/). Sembrava più interessante Data Parallel Haskell dato che permetteva nested data parallelism, ma il progetto [è stato interrotto nel 2010](https://gitlab.haskell.org/ghc/ghc/wikis/data-parallel).
- con strategie. Implementato nella libreria [Control.Parallel](http://hackage.haskell.org/package/parallel) di Haskell.
- dataflow. Implementato nella libreria [Control.Monad.Par](http://hackage.haskell.org/package/monad-par) di Haskell.

È interessante anche il parallelismo implicito (ho trovato FDIP), ma non penso lo testerò. Potrei però fare un paragone con risultati di altri.

Altri framework:
- [Eden](http://www.mathematik.uni-marburg.de/~eden/): parallelismo con thread espliciti, in qualche modo simile alle strategie, ma parallelizza a livello di funzioni invece che di thunk da valutare.
- [Accelerate](https://www.acceleratehs.org/get-started.html): flat data parallelism, molto simile a Repa.
- [haskell-cnc](http://hackage.haskell.org/package/haskell-cnc): parallelismo dataflow, simile a Control.Monad.Par ma più avanzato.

# Uso
L'intero progetto è basato su [`stack`](https://docs.haskellstack.org/en/stable/README/), che è la sua unica dipendenza.
Per eseguire la benchmark suite basta usare
```
stack run
```
L'esecuzione avviene tramite [`criterion`](http://www.serpentine.com/criterion/); per generare l'output completo in formato html basta eseguire
```
stack run -- --output results.html
```
È anche possibile verificare che i benchmark della suite calcolino lo stesso risultato con
```
stack test
```

# Benchmarks
Elenco dei benchmark, in cui viene spiegato cosa testano e/o perché li ho scelti.

## Interessanti
Questi benchmark secondo me coprono quasi tutti i tipi di parallelismo comuni, manca solo un esempio di parallelismo dataflow (possibilmente con grafo dinamico).

- `Queens`: problema delle regine. Paradigma divide-et-impera.
- `Matmult`: moltiplicazione di matrici (algoritmo O(N^3) ingenuo). Data parallelism con clustering.
- `Coins`: conta il numero di modi per realizzare una certa somma con monete di certi tipi. Paradigma divide-et-impera con soglia (clustering esplicito).
- `Minimax`: ricerca alpha-beta su un albero per un gioco a due giocatori. Paradigma divide-et-impera con pruning di rami inutili (con lazyness? GC? Esplicito? Dovrebbe dipendere dal tipo di parallelismo).
- `Nbody`: problema degli n corpi. Flat data parallelism.
- `Sphere`: raytracing. Flat data parallelism.
- `Transclos`: calcola tutti gli elementi raggiungibili da un insieme iniziale tramite una relazione data. Calcola in parallelo una lista infinita con data parallelism o producer-consumer.

- Un gate array simulator. Si presta bene a parallelismo speculativo e dataflow. `Circsim` in nofib/spectral fa molto più del necessario e probabilmente fa troppo, nel senso che non si riesce a parallelizzare come vorrei.

## Scartati
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
Benchmark utilizzati in alcuni articoli ma che non ho trovato, e che ritengo vengano adeguatamente sostituiti da altri.

- `Genetic`: allinea sequenze di DNA, usando divide-et-impera e nested data parallelism.
- `Maze`: esplorazione di un labirinto 2D. Parallelismo speculativo.

## Altre note
Tutti i miei benchmark vengono da [nofib](https://gitlab.haskell.org/ghc/nofib). Ho cercato anche altri benchmark suites, ma tutte le altre trovate (es: haskell-CnC di Intel) contenevano molti benchmark in comune con nofib, e non ho trovato motivi particolari per includere quelli non in comune.

# Crediti
Tutto il contenuto della cartella `src` è basato su [nofib](https://gitlab.haskell.org/ghc/nofib) se non altrimenti specificato, e i crediti per il contenuto vanno anche agli autori originali.
