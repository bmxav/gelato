# Gelato
Gelato is a personal experiment on compiling a language to Go. The goal is to allow
for a simple, yet expressive, language that allows for interop with Go
libraries.

## Example
```
let x = 5

# `let` bindings are immutable, so this would be an error:
#x = 20

# `var` on the other hand is mutable:
var y = 1
y += 10

# `if` statements are expressions, so they can generate a value:
let z = if x > y then
    x - y
else
    y - x
end
```

The above would generate the following Go code:
```go
{
	x := 5
	y := 1
	y += 10
	var z int
	if x > y {
		z = x - y
	} else {
		z = y - x
	}
}
```
