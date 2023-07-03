# Witness Generation Tool for zkWrapper

## Building

To build the CEK machine emulator, you need to install Haskell (GHC 8.10.7 and Cabal 3.6.2.0). Use
```console
cabal new-build
```
to build the emulator.

## Testing

The test folder contains files `constants.json` and `terms.json` created by our UPLC parser. These two files are needed to generate the witness for our ZKP protocol. Use
```console
cd test
./run.sh
```
to run the CEK machine emulator on these input files.
