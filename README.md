# Big Integer Calculator [![Build Status](https://travis-ci.org/kortescode/Big-Integer-Calculator.svg?branch=master)](https://travis-ci.org/kortescode/Big-Integer-Calculator)

Big Integer Calculator is a OCaml executable. It performa arithmetis operations with 100+ digit numbers.

## Requirements

Building the Big Integer Calculator executable requires the following tools:
- OCaml programming language (see [www.ocaml.org/](https://ocaml.org/))
```bash
sudo apt-get install ocaml-nox
```
- Menhir, an LR(1) parser generator (see [INRIA GitLab page](https://gitlab.inria.fr/fpottier/menhir/))
```bash
sudo apt-get menhir
```

## Compilation

To build the executable, use:

```bash
make all
```

## Usage

```bash
./bistro [-obase (2|8|10|16)] [inputfile]
```

The `-obase` option defines the environment arithmetic base.

### Example

```
./bistro

4534 * 6645 / (30120000 + 8430) % 26 + 41
= 42

(42 * (8956 - 128)) * (20456 / (10243 + 256))
= 370776

((42 + 256) / 128) + (20456 - 8956 * 10243)
= -91715850

54949424216216125424516262623121156016262 * 24154945672452261615614124453226212156212232
= 1327300356675233206412620059717388929665022797546176463230478947455367247264515316784
```

## License

Distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/). See [`LICENSE`](LICENSE) for more information.
