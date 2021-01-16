# An Anydice interpreter

Diceprob is an interpreter for the scripting language used by
[AnyDice](https://anydice.com/). It should make it possible to run AnyDice
scripts locally.

## Installation

Via [stack](https://docs.haskellstack.org/en/stable/README/).

## Example

Given a file `test.ad`, containing:

```
X: 2
output X + d6
X: X * 2
output X + d6 * d4
```

Diceprob should output:

```
"output 1",5.499999999999999,27.666666666666664,3,8
#,%
3,0.16666666666666666
4,0.16666666666666666
5,0.16666666666666666
6,0.16666666666666666
7,0.16666666666666666
8,0.16666666666666666

"output 2",12.749999999999998,186.99999999999997,5,28
#,%
5,4.1666666666666664e-2
6,8.333333333333333e-2
7,8.333333333333333e-2
8,0.125
9,4.1666666666666664e-2
10,0.125
12,8.333333333333333e-2
13,4.1666666666666664e-2
14,4.1666666666666664e-2
16,0.125
19,4.1666666666666664e-2
20,4.1666666666666664e-2
22,4.1666666666666664e-2
24,4.1666666666666664e-2
28,4.1666666666666664e-2
```

In general, output produced by Diceprob and AnyDice should be identical (but it
most likely still isn't for some corner cases).

## Status

### Language Features

The following language features are not yet implemented:

| Feature          | Status  |
| ---------------- | ------- |
| Arbitrary dice   | missing |
| Functions        | missing |
| Standard Library | missing |

### Output

Currently, Diceprob can only output results equivalent to what AnyDice produces
when "View" is set to "Export" and "Data" is set to "Normal".
