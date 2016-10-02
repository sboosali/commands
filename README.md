[![Build Status](https://secure.travis-ci.org/sboosali/commands-server-simple.svg)](http://travis-ci.org/sboosali/commands-server-simple)
[![Hackage](https://img.shields.io/hackage/v/commands-server-simple.svg)](https://hackage.haskell.org/package/commands-server-simple)

# commands-server-simple

A very simple `servant` HTTP server that executes `WorkflowT IO` actions.

e.g.

```haskell
import Commands.Servers.Simple
import Workflow.<platform> -- Workflow.OSX, Workflow.Windows, Workflow.Linux
main = runSimpleServer (defaultSettings runWorkflowT)
```
