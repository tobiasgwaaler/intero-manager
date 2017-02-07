## TODO

### Parsing
- ☐ Parse error/warn messages from Intero/GHC/GHCi

### Interface
- ☐ Accept JSON input for commands and JSON output for results
- ☐ Use stdin stdout for communication
- ☐ Support communication over socket

### Validate environment
- ☐ Error message if Stack is not installed
- ☐ Error message if current project/file is not managed by Stack (ie. there is no stack.yaml)

### Install Intero
- ☐ Install Intero using `stack build intero` if it's not already installed for the given resolver
- ☐ Prompt the user before installing Intero
- ☐ Display progress information when installing Intero

### Multiple targets
- ☐ List available targets
- ☐ Set/change target

### Cabal
- ☐ Restart Intero when the cabal-file has changed (to install new packages etc.)
- ☐ Restart Intero when stack.yaml has changed (ie. to install extra deps, get new targets etc.)
