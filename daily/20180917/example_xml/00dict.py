import xmltodict

doc = {
    "entry": {
        "@xmlns": "http://purl.org/atom/ns#",
        "title": "Sample",
        "content": {
            "@mode": "base64",
            "@type": "image/jpeg",
            "#text": "/9j/2wCEAAQDAwQDAw.../9n/AA=="
        },
    },
}
print(xmltodict.unparse(doc, full_document=False))
"""
<entry xmlns="http://purl.org/atom/ns#">
  <title>Sample</title>
  <content mode="base64" type="image/jpeg">/9j/2wCEAAQDAwQDAw.../9n/AA==</content>
</entry>
"""
