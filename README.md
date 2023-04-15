## Crafting Interpreters Part III - CLox

The C implementation of `A Bytecode Virtual Machine` from Part III of [Crafting Interpreters](https://craftinginterpreters.com/).

For integration with `clangd` in editors, the `compile-commands.json` generated during the configuration step can be simlinked. At project's root, run:

```
$ cmake -S . -B build
$ ln -s build/compile-commands.json .
```

Invoke the build system to compile the project (assumes the configuration step has been performed).

```
$ cmake --build build
```

VSCode `launch` configurations have been created to launch the interpreter with either Ninja and Make.
