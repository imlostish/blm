## TODO Tasks

- [ ] [`main.cbl`](./src/core/main.cbl) Accept command line arguments and pass them to the CLI.
- [ ] [`args-auth.cbl`](./src/core/auth/args-auth.cbl) Implement argument parsing for authentication authorization and other CLI commands.
- [ ] Makefile - Compilation for the project
  - [ ] Generate C source files from CBL files.
  - [ ] Generate ASM source files from CBL files.
  - [ ] Compile into [`bin`](./build/bin) and [`sbin`](./build/sbin) directories from CBL files for project build verification.
  - [ ] Compiled for [release](./build/release/) and [debug](./build/debug/). With build safety and organization for `.cbl` files and dedicated tests for behavior logging and error detection.
- [ ] Finish developing the terminal user flow in [cli.cbl](./src/core/cli.cbl)
- [ ] Access and query of persistent data
- [ ] Pwd flow with SHA-256.
- [ ] Fix segmentation fault (core dumped) ./build/sbin/blm_single
