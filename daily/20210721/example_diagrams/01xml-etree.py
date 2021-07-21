from xml.etree import ElementTree

t = ElementTree.fromstring("""
<doc>
 <item>xxx</item>
 <item>xxx</item>
 <item>xxx</item>
</doc>
""")

print(t.findall("item"))
print("-------------")
t = ElementTree.parse("architecture.svg")
ns = {"svg": "http://www.w3.org/2000/svg", "xlink": "http://www.w3.org/1999/xlink"}
# for node in t.iterfind("[@xlink:href]", ns):

# /svg/g/g[1]/image
print(t.findall("svg:g/svg:g[1]/svg:image", ns))
print(t.findall("//svg:image", ns))
