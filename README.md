# ImportSchema

Generates very simple ODBC wrapper functions for ODBC tables.

# Building

`importschema` is set up to be built with `stack`.

    stack build

Should download and install any dependencies, and then build the
project.

    stack exec importschema -- [CMD_LINE_OPTS]

To run the utility.

# Usage

    importschema -c CONN_STRING -t TABLE_NAMES [-o OUTPUT_FILE]

