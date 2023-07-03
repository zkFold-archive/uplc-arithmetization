## Example

```
aiken uplc eval fib.uplc "(con pair<integer, integer> [10, 55])"
```

result:

```
{
  "result": "(con bool True)",
  "cpu": 16252423,
  "mem": 37627
}
```