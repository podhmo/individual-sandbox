import pydoc

from collections import Mapping
print(pydoc.plaintext.document(Mapping.keys))
