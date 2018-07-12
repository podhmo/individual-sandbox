import tokenize
from io import BytesIO

source = b"(1 + 2) * 3"
for tk in tokenize.tokenize(BytesIO(source).readline):
    print(tk.exact_type, tk)
