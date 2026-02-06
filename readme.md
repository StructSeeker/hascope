## Introduction 
hascope is an implementation of Kaleidoscope, the famous toy language of llvm, an expression language with simple control flow and types.

## Notes
Quite old GHC version (8.10.*) is required because the used llvm binding is no longer actively maintained and not compatible with modern GHC due to bytestring issues. I tried but failed to find an up-to-date llvm binding

Also because llvm-hs is not maintained, one must install LLVM version 9.0.x to run JIT.

llvm-hs use llvm-config to checkup llvm installation, but llvm 9.0.1 don't ship a llvm-config binary, so this maybe hard for windows user.

This implementation mainly follows the tutorial by Stephen Diehl, i attempted to refactor with ghc2024's more advanced feature, but llvm binding is quite picky and needy on some old libraries. It's seems write a to-llvm compiler in haskell is not the most popular idea, maybe i should have used Ocaml.

## Usage
### Building

```bash
cabal build
```

### Running

The compiler supports both JIT execution and compilation modes:

```bash
# Run the REPL for interactive evaluation
cabal run hascope

# Compile a source file
cabal run hascope -- input.ks -o output
```

### Example

```kaleidoscope
# Define a function
def fib(x)
    if x < 3 then
        1
    else
        fib(x-1) + fib(x-2);

# Call it
fib(10);
```



