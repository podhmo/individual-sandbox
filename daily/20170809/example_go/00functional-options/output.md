```
default
&main.Config{
  DB: main.DB{
    URL:  "http://localhost:8888",
    Name: "testdb",
  },
  LogLevel: 0x01,
}
custom
&main.Config{
  DB: main.DB{
    URL:  "http://me.com/db",
    Name: "testdb",
  },
  LogLevel: 0x00,
}
```
