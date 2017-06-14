
PosetLogic is a model checker on Kappa causal flows (stories). It is designed to be used as an accompaying analysis tool for a KaSim simulation. Users can write influence statements about a Kappa model and PosetLogic will query the pool of stories to check if there exists a 'mechanistic' explanation of the influence statement.

## Installation

To compile PosetLogic you need

- the OCaml native compiler version 4.02.3 or above as well as _ocamlbuild_, _findlib_ and _Yojson_ library.
- the KappaLib that you can get from https://github.com/Kappa-Dev/KaSim

If you use opam KappaLib, OCaml compilers, ocamlbuild and findlib are provided by it.

## Usage

You first need a pool of Kappa stories in a json format. You can produce them by running a KaSim simulation (please visit Kappa's documentation on http://dev.executableknowledge.org/docs/KaSim-manual-master/KaSim_manual.htm). Or you can use the stories provided in the example set.

Then you need to write a formula file. Each line in the file is either a formula or a variable assignation.

## Run the example

In the folder [examples] you'll find the set of stories produced by the model test3.ka with the simulation options of inputs.ka. The stories are both in a json format (necessary for PosetLogic) and in a dot format, for visualisation. The files test1 and test2 are files with formulas.

You can test whether the model in test3.ka satisfies the formulas in the files test1 and test2 by typing

'./main.native examples/test3/c0.json examples/test3/c1.json examples/test3/c2.json examples/test3/c3.json examples/test3/c4.json examples/test3/c5.json -env examples/test3/env.json -f examples/test3/test2'

Alternatively you can use the script 'script_test3'.
