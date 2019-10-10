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

```bash
./bistro
38 + 4
= 42
4534 * 6645 / (30120000 + 8430) % 26
= 1
```

## License

Distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/). See [`LICENSE`](LICENSE) for more information.
