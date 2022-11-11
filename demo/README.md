# Miking - sketch of externals

This is a sketch of how support for externals could be implemented using
dynamic loading of the dependent libraries. The approach is as follows:
1. We generate an intermediate file (`extdyn.ml`) which declares all the
   externals we desire using the new `add_external` intrinsic.
2. This is compiled as a library (see configuration in `dune`) which is
   statically linked with the external OCaml libraries _and_ the `boot`
   library.
3. In the generated OCaml code (`program.ml`), we make the following changes
   compared with the current code generation:
  1. At the start of the file, we use the new `load_intrinsics` function of
     boot to load the externals we need. The first argument is the list of
     libraries we wish to load dynamically - the same as we link statically
     with the intermediate file. These are loaded using
     `Fl_dynload.load_packages`, as this correctly handles dependencies of the
     libraries. However, this function requires the libraries to be installed,
     so we cannot do this for the intermediate file. Therefore, the second
     argument is the path to the generated `extdyn.cmxs` file, which we load
     using `Dynlink.loadfile`.
  2. All references to a variable in the MExpr program are replaced with a call
     to `get_external`, providing the string identifier.

To support this in the interpreter (`mi` and possibly also in `boot`), we would
need to do steps 1, 2, and 3.1 at the start of the interpreter, while step 3.2
would be applied whenever an external identifier is encountered in the
interpreter.

The `load_intrinsics` makes use of the `Fl_dynload.load_packages` to correctly
load the dependent libraries _and_ their dependencies dynamically. Because of
this, we need to add `findlib.dynload` as a dependency of the `boot` library.

## New intrinsics

Theree new intrinsics are added to support this:
* `add_external : string -> 'a -> unit` and `get_external : string -> 'a` to
  add new externals and to load them, using a global hashtable. For the
  approach described above, the `add_external` would not be needed as an
  intrinsic, as it only runs in the intermediate file (which we probably want to
  generate directly). However, I imagine it could be useful in other cases, for
  example for doing just-in time compilation.
* `load_libraries : string list -> string -> unit` to load the external
  library dependencies as well as the intermediate library. This intrinsic is
  necessary since we want to do this in the interpreter.

## Example

The example program `program.ml` is derived from the result of compiling
`stdlib/ext/dist-ext.mc` with tests enabled. To run it, use the command
`dune exec ./demo/program.exe` from the root of the repo.

## Limitations

Specifically for the `owl` library, it seems the dynamic loading fails by
default because its C libraries are not properly loaded. A workaround is to
statically link the `eigen` library to the executable. However, this means that
to support `owl` in `mi eval` or `boot eval`, we would need to statically link
this library when compiling `mi` or `boot`, respectively.
