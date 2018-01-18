## python argparse 相互排他

相互排他(mutual_exclusive)の設定は`--foo`みたいなoptionだけに限定されない。

こういうこともできる。

```python
group = parser.add_mutually_exclusive_group()
group.add_argument('src', nargs="?")
group.add_argument('--format', choices=["json", "yaml"])
```

