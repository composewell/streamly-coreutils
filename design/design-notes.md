## Input and output objects

The utilities must be written as Haskell functions that take an input object
and produce an output object. The input is usually the "config" for the request
and output is the serialized data (e.g. json or Haskell structure).

For raw haskell functions, the config would be a Haskell structure and the
output would also be a Haskell structure.

Traditionally the config is specified using the CLI arguments or a config file.
And output is more of a human readable format.

We can write adaptors for the Haskell functions to:

* parse the CLI arguments or config files into a Haskell input structure
* render the Haskell output to json or human readable format of traditional
  utilities.

The parsing can be done statically when possible e.g. using the TH quotes as
sugegsted above.

We can basically wrap the Haskell functions in CLI parser and an output
renderer to make a Unix like utility.

## Module structure

Each command must have its own module e.g. Coreutils.Cp for the cp command.

The command options could be called "Options" in each module but then
we may not be able to export all commands from a single module which
is desirable. So we can choose to call the options of a command by the
same name as the command itself e.g. "Cp".  We would not expose the Cp
structure outside the module but we would still need to export the
type. Using a unique type for each command would help us use it
unqualified.

The command runner would be called "cp" so it would be "cp :: Cp -> IO ()".
