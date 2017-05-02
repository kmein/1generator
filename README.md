# 1generator

A translation of [1generator](https://github.com/greatestview/1generator) into a
Haskell command line application. It reads `stdin` and outputs to `stdout`.

Example usage:

```bash
$ echo "Was ist das für ein Leben" | stack exec 1generator-exe
Was is das für 1 Lebm

$ echo 'Was ist das für ein Leben' | stack exec 1generator-exe
Was is das führ 1 Lebem
```
