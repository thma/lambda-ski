# Evaluating SKI combinators as native Haskell functions

## abstract

## Introduction

 I will show you how to compile a program to a finite, fixed set of combinators (SKI), and then evaluate these combinators as normal Haskell function. This technique was introduced in Matthew Naylor’s Evaluating Haskell in Haskell.

The source code is available here.


## performance

	SICKBY GraphReduction	SICKBY asunctions	Haskell native 	G/HHI	HHI/Native
factorial	1420	32,1	4,31	44	7
fibonacci	420	41,9	3,04	10	14
ackermann	450	20,9	0,405	22	52
gaussian sum	1420	20	1,65	71	12
tak	3610	112	0,781	32	143
					
	7320	226,9	10,186	32	22


## Related ideas

https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf

https://smunix.github.io/kseo.github.io/posts/2016-12-30-write-you-an-interpreter.html

