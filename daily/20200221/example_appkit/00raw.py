from appkit import Config, Error, Cleanup
from prestring.go import Module


"""
package conf

type Config struct {}
func LoadConfig(filename string) (*Config, error) {}
"""

"""
package foo

func FromConfig(c Config) *Foo {
}
"""

m = Module()

with m.func("run", return_="error"):
    m.stmt("filename := *config")
    m.stmt("c, err := conf.LoadConfig(filename)")
    with m.if_("err != nil"):
        m.return_("err")

    m.stmt("fooOb := foo.FromConfig(c)")
    m.return_("use(fooOb)")
print(m)
