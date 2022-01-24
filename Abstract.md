---
title: IN450, Implementazione Standard di SHA-3
author: Andrea Pelosi, 563003
---

Il progetto svolto (si consulti il file `Keccak.wl`) consiste in una implementazione in Mathematica di KECCAK, famiglia di algoritmi scelti dal *National Insitute of Standard and Technology* (NIST) per rappresentare lo standard *Secure Hash Algorithm 3* (SHA-3). L'implementazione segue la specifica presentata nel documento \emph{FIPS PUBLICATION SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions} [(link all'articolo)](https://csrc.nist.gov/publications/detail/fips/202/final).
Le quattro funzioni di hash SHA-3 sono denominate SHA3-224, SHA3-256, SHA3-384, SHA3-512 (il suffisso indica il numero di bit prodotti come digest della funzione) e sono ottenute a partire da KECCAK istanziato con gli opportuni parametri.

KECCAK è una famiglia di \emph{sponge function} che usa la permutazione KECCAK-$f$ come funzione sottostante e multi-rate padding come regola di padding. Una sponge function è un costrutto in grado di ricevere input e produrre output di lunghezza arbitraria. Essa è definita da una funzione sottostante su input di lunghezza fissata, una regola di paddding e un rate.

Nel progetto le sponge function usano come funzione sottostante KECCAK-$p$, una generalizzazione di KECCAK-$f$.
La permutazione KECCAK-$p[b,n_r](S)$ applica all'input $S$ di lunghezza $b$ sequenzialmente le permutazioni denominate $\theta, \rho, \pi, \chi, \iota$ un numero di volte che è funzione di $n_r$ (ognuna di queste applicazioni sequenziali è chiamata \emph{round}).
Si osservi che la funzione KECCAK-$f[b]$ è equivalente a KECCAK-$p[b,12+2\ell]$ con $\ell= log_2{(\frac{b}{25})}$. In particolare la funzione sottostante alle funzioni hash di SHA-3 è KECCAK-$p[1600,24]$ (equivalente a KECCAK-$f[1600]$).

Nel progetto sono state implementate alcune utilities per compiere operazioni su liste di bit che il linguaggio Mathematica non mette a disposizione nativamente.

Data in input una stringa esadecimale $M$ di lunghezza pari, l'applicazione ne calcola il digest $H(M)$, dove la funzione $H$ è una tra le funzioni SHA3-224, SHA3-256, SHA3-384, SHA3-512. La lunghezza della stringa in input deve essere pari perché l'applicazione si aspetta un messaggio $M$ che sia allineato al byte (ovvero tale che se $\ell(M)$ è la lunghezza in bit di $M$, allora $\ell(M)=8m$, per qualche $m$ intero non negativo).

Di seguito un esempio d'utilizzo dell'applicazione:

```Mathematica

(* Genero una stringa esadecimale causale *)
In[55]:= M = RandomHexString[2^1000]

Out[55]= "fca6b6ecdca3ed0a810af063d56843b0f376b7fa634f5c436995f487f9cab2853ee64\
fd66fe64878c4e720a37e64333f7c63b58ae6c8232d894c3e89e3b9304f2f8d067e538\
c85b7acadb743c60313f04d864a8261a43ff304dd6bb7cf8c46742fc63d467c65cc464\
8837fdd0e45d36ec5f8865a2b5b63a0a466566bd5"

(* Controllo che la stringa non abbia lunghezza dispari i.e. sia byte-aligned *)
In[56]:= StringLength@M

Out[56]= 250

(* Calcolo il digest di M con le funzioni hash SHA3 *)

In[57]:= SHA3224[M]

Out[57]= "3650a7633628439a2d3460525fa10f1f99d01e1ba61e0ecba16be0f7"

In[58]:= SHA3256[M]

Out[58]= "5e0173208d8f093dbcfc8e5c8c6e43ebafcecaa173f813d3a064211de99f\
20f8"

In[59]:= SHA3384[M]

Out[59]= "6206458c905a4e5d092687342b1afcbc564135f82a6704086dca0e9e395b\
1824afccaa13a6af1c3e403d1dedc286b417"

In[60]:= SHA3512[M]

Out[60]= "1e5d09d369b606b2cead0c1be99d7024f97b20d8580c18eeb4d538a0c539\
e09e5a0af14eb0743dc1477044e6796a0360a10e74e6298545a7f615e2be2460f97f"
```

