```console
$ cue --help
cue evaluates CUE files, an extension of JSON, and sends them
to user-defined commands for processing.

Commands are defined in CUE as follows:

        command deploy: {
                cmd:   "kubectl"
                args:  [ "-f", "deploy" ]
                in:    json.Encode($) // encode the emitted configuration.
        }

cue can also combine the results of http or grpc request with the input
configuration for further processing. For more information on defining commands
run 'cue help cmd' or go to cuelang.org/pkg/cmd.

For more information on writing CUE configuration files see cuelang.org.

Usage:
  cue [command]

Available Commands:
  cmd         run a user-defined shell command
  eval        evaluate and print a configuration
  export      output data in a standard format
  fmt         formats CUE configuration files
  get         add dependencies to the current module
  help        Help about any command
  import      convert other data formats to CUE files
  trim        remove superfluous fields
  version     print CUE version
  vet         validate data

Flags:
      --debug            give detailed error info
  -h, --help             help for cue
  -i, --ignore           proceed in the presence of errors
  -p, --package string   CUE package to evaluate
  -s, --simplify         simplify output
      --trace            trace computation
  -v, --verbose          print information about progress

Use "cue [command] --help" for more information about a command.
```
