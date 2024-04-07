# fg-bytegen
fg-script lang bytecode/IR emitter

# Building
1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Run the following commands
```bash
# stack install HUnit
# stack install parsec

stack test
# stack build --fast
stack run debug "fn add(num a, num b) -> str { ret str(a + b); }"
```