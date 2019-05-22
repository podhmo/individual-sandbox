## 00

1. parse function definitions
2. capture toplevel variable
3. calling init function

## 01

https://golang.org/ref/spec#Composite_literals

```
xs := [...]int{1, 2}
// same as xs := [2]int{1, 2}
```

## 02

https://golang.org/ref/spec#Composite_literals

1. 

`[]int{010: 10}`

```go
// vowels[ch] is true if ch is a vowel
vowels := [128]bool{'a': true, 'e': true, 'i': true, 'o': true, 'u': true, 'y': true}

// the array [10]float32{-1, 0, 0, 0, -0.1, -0.1, 0, 0, 0, -1}
filter := [10]float32{-1, 4: -0.1, -0.1, 9: -1}
```
