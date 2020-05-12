"""
// expected code

func (g *Greeter) Hello(name string) string {
	return fmt.Sprintf("Hello %s\n", name)
}
"""

from prestring.go import Module

m = Module()
with m.method("g *Greeter", "Hello", "name string", returns="string"):
    m.stmt('return fmt.Sprintf("Hello %s\\n", name)')
print(m)
