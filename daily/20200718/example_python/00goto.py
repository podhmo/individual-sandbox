import re
import unicodedata

s = """
GO TO キャンペーン
GO TOキャンペーン
GOTO キャンペーン
GOTOキャンペーン
Go To キャンペーン
Go Toキャンペーン
Go to キャンペーン
Go toキャンペーン
GoTo キャンペーン
GoToキャンペーン
Goto キャンペーン
Ｇotoｷャンペーン
ＧO TO ｷャンペーン
ＧO TOｷャンペーン
ＧOTO ｷャンペーン
ＧOTOｷャンペーン
Ｇo To ｷャンペーン
Ｇo Toｷャンペーン
Ｇo to ｷャンペーン
Ｇo toｷャンペーン
ＧoTo ｷャンペーン
ＧoToｷャンペーン
Ｇoto ｷャンペーン
Ｇotoｷャンペーン
GO　TO　キャンペーン
GO　TOキャンペーン
GOTO　キャンペーン
GOTOキャンペーン
Go　To　キャンペーン
Go　Toキャンペーン
Go　to　キャンペーン
Go　toキャンペーン
GoTo　キャンペーン
GoToキャンペーン
Goto　キャンペーン
Gotoキャンペーン
GO	TO	キャンペーン
GO	TOキャンペーン
GOTO	キャンペーン
GOTOキャンペーン
Go	To	キャンペーン
Go	Toキャンペーン
Go	to	キャンペーン
Go	toキャンペーン
GoTo	キャンペーン
GoToキャンペーン
Goto	キャンペーン
Gotoキャンペーン
"""

rx = re.compile(r"\s+")
for line in s.strip().split("\n"):
    print(unicodedata.normalize("NFKC", rx.sub("", line.lower().strip())))
