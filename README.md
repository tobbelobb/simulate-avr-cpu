## Synopsis

This code is meant to simulate the state of an AVR microprocessor like the ATmega 328 or similar.

## Code example

Modifying the processors state is supposed to be done with function/macro calls looking like:

```
(ldi r20 1)
(ldi r21 41)
(add r21 r20)
(print-register r21)
 ==> 00110100 00110010
```

## Motivation

Libraries of really good AVR assembly would make cheap CPUs applicable in a wider range of situations. However, writing AVR Assembly is really hard work. This tool is meant to facilitate automation of AVR Assembly code generation. The reduced instruction set and small memory of AVR CPUs makes automatic assembly feasible to both write and test.

## Inspiration

I found out about "stochastic superoptimization" in [this presentation](https://www.youtube.com/watch?v=aD9mZDJzb58). Also check out their papers [one of them here](http://theory.stanford.edu/~aiken/publications/papers/asplos13.pdf) and [STOKE repo](https://github.com/StanfordPL/stoke-release).

## Why Lisp?

Macros, flexible type system and performance.

### Performance?

Optimizing most often starts with profiling, so maybe [this blog post](http://john.freml.in/sbcl-optimise-profiling) for a little on per-function profiling. For statistical profiling, see [this blog post](http://t-b-o-g.blogspot.se/2009/12/brians-brain-on-common-lisp-take-3.html). To see how SBCL can be sometimes be used for headless 'gcc -O3'-style compilation, check [this blog post](http://blog.30dor.com/2014/03/21/performance-and-types-in-lisp/) and [these blog posts](http://nklein.com/tags/optimization/). 

See also the excellent [SBCL manual](http://www.sbcl.org/manual/).

## Licence
GPLv3
