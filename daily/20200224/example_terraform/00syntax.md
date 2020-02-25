# terraformの設定ファイルのsyntaxを知りたい

- https://www.terraform.io/docs/configuration/syntax.html

arguments (attributes)

```
image_id = 123
```

blocks

```
resource "aws_instance_" "example" {
  ami = "abc123"
  
  network_interface {
    # ...
  }
}
```

identifiers (一応unicodeも使えるみたいだけれど)

```
[a-zA-Z][a-zA-Z0-9_\-]+
```

comments

```
# begins a single-line comment, ending at the end of the line.

// also begins a single-line comment, as an alternative to #.

/* and */ are start and end delimiters for a comment that might span over multiple lines. 
```

