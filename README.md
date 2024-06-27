# isabelle-utils

## What is this tool for?

This tool (isa-utils) contains utility functions for parsing [Isabelle](https://isabelle.in.tum.de/) theories.
Main features include:

- parse theories, returning a list of commands
- get imports of theories, returning a list of theory paths
- extract constants from theories, returning a list of constants

## How to use it?

This tool can either be used as a Scala library or as a single jar file.

**As a Scala library**

`IsabelleProcess` is the main class to use.
To create an instance, you need to pass the path to the Isabelle distribution, the session name, the working directory, and optional the path to the [Archive of Formal Proofs (AFP)](https://www.isa-afp.org/) entry folder.

It provides the following methods:

- `parse` and `parseBatch` to parse a theory or a list of theories
- `getImports` to get the imports of a theory
- `extractConstants` to extract the constants of a theory

**As a single jar file**

The jar file can be built by running `sbt assembly`, using the [`sbt-assembly`](https://github.com/sbt/sbt-assembly) plugin.
It is just a wrapper around the `IsabelleProcess` class, providing a few command-line options.
Some most useful options are:

- `TASK`, the task to perform, accepting `parse`, `parse_batch`, `get_imports`, `get_imports_batch`, `extract_constants`
- `SESSION`, the session name, e.g., HOL, HOL-Analysis, etc.
- `ISA_HOME`, path to the Isabelle distribution, e.g., `/path/to/Isabelle2023`
- `FILEPATH*`, the path(s) to the theory file(s)
- `--afp`, the path to the AFP entry folder
- `--output`, optional output file path

**NOTE**

This tool uses [`scala-isabelle`](https://github.com/dominique-unruh/scala-isabelle) to interact with Isabelle.
Currently, it only supports Isabelle2021 to Isabelle2023 and is only tested on Isabelle2023.
It is known that it *does not* work with Isabelle2024.

Additionally, the constant extraction utility heavily relies on the underlying Isabelle/ML code, which may contain bugs.
If you encounter any issues, please report them.
