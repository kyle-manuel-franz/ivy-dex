# Ivy DEX
Official Ivy DEX github repo started from the Plutus starter project from IOHK.

# Running Tests

Start the nix shell: 

`nix-shell`

Load up cabal repl:

`cabal repl`

Load the Tests.Spec.Order test module into the repl:

`:l Tests.Spec.Order`

Import Tasty.Test module into the repl:

`import Tasty.Test`

Run the tests:

`defaultMain tests`

You should see something like this (tests may differ from this output):
```
order
  Can place an order:                               OK (0.16s)
  Can place and take order:                         OK (0.11s)
  Can place and not take order if wrong value paid: OK (0.09s)
  Can place and cancel order:                       OK (0.09s)
  Can place and non owner cannot cancel order:      OK (0.07s)

All 5 tests passed (0.53s)
```

# Build new plutus file
Load up nix shell env
`nix-shell`

Run cabal repl
`cabal repl`

Load the Deploy.Order module
`:l Deploy.Order`

Run the write order validator function with an integer passed as the version number
`writeOrderValidator <version>`

You should see a file in the `dist/testnet` folder called `order_v_<version>.plutus`

# Plutus Platform starter project
![CI](https://github.com/input-output-hk/plutus-starter/actions/workflows/test.yml/badge.svg?branch=main)


This project was forked from the IOHK plutus starter app project found here:

https://github.com/input-output-hk/plutus-starter


