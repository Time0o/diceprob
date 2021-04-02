# An Anydice interpreter

Diceprob is an interpreter for the scripting language used by
[AnyDice](https://anydice.com/). It should make it possible to run AnyDice
scripts locally. It produces text output in the "normal" data format and
supports all major AnyDice features except function calls.

## Installation

Via [stack](https://docs.haskellstack.org/en/stable/README/).

## Example

Given a file `test.ad`, containing:

```
loop M over {1..3} {
  \ roll two dice of size 2/4/6 \
  N: M * 2
  output 2dN named "two [N] sided dice"
}
```

Diceprob should output:

```
"two 2 sided dice",3.0,6.5,2,4
#,%
2,0.25
3,0.5
4,0.25

"two 4 sided dice",5.0,22.5,2,8
#,%
2,6.25e-2
3,0.125
4,0.1875
5,0.25
6,0.1875
7,0.125
8,6.25e-2

"two 6 sided dice",7.0,47.833333333333336,2,12
#,%
2,2.7777777777777776e-2
3,5.555555555555555e-2
4,8.333333333333333e-2
5,0.1111111111111111
6,0.1388888888888889
7,0.16666666666666669
8,0.1388888888888889
9,0.1111111111111111
10,8.333333333333333e-2
11,5.555555555555555e-2
12,2.7777777777777776e-2
```

In general, output produced by Diceprob and AnyDice should be identical (but it
most likely still isn't for some corner cases).
