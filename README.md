## Crafting Interpreters Part III - CLox

The C implementation of `A Bytecode Virtual Machine` from Part III of [Crafting Interpreters](https://craftinginterpreters.com/).

CMake is used for building.

For integration with `clangd` in editors, the `compile-commands.json` generated during the configuration step can be simlinked. At project's root, run:

```
$ cmake -S . -B build
$ ln -s build/compile-commands.json .
```

Invoke the build system to compile the project (performed after the configuration step).

```
$ cmake --build build
```

VSCode `launch` configurations have been created to launch the interpreter after building with either `Ninja` or `Make`.

The resulting executable is in the `bin` subdirectory of the directory used for building ("build", in the example above).

```
$ ./build/bin/clox_bytecode_interpreter
```

To run the REPL, or provide a path to a `lox` file to execute a script.
