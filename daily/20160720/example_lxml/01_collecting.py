import lxml.html
from lxml.cssselect import CSSSelector
# pip install lxml cssselect
html = """
<div class='a'><img src='aa.jpg'></div>
<div class='b'><img src='ab.jpg'></div>
<div class='b'><img src='ac.jpg'></div>
<div class='b'><img src='ad.jpg'></div>
<div class='a'><img src='ba.jpg'></div>
<div class='b'><img src='bb.jpg'></div>
<div class='b'><img src='bc.jpg'></div>
<div class='a'><img src='ca.jpg'></div>
<div class='b'><img src='cb.jpg'></div>
"""


def collecting(dom):
    selector = CSSSelector(".a,.b")
    root = lxml.html.Element("root", {})
    parent = None
    for div in selector(dom):
        if div.get("class") == "a":
            parent = div
            root.append(div)
        else:
            for img in div:
                parent.append(img)
    return root

dom = lxml.html.fromstring(html)
dom = collecting(dom)
print(lxml.html.tostring(dom, pretty_print=True).decode("utf-8"))
