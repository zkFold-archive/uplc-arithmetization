# Translator

program on base of flex and bison to translate uplc code to signals

# How to use

```bash
make
```

Takes exmample from test.

Output is in "consts.json" and "terms.json".

Binary file "translator" is generated.

We can use it as:

```bash
./translator ./test/fib.uplc consts.json terms.json
```