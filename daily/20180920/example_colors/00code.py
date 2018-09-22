from prestring.python import Module
import matplotlib.cm as cm

m = Module()  # noqa
m.from_('nbreversible', 'code')
m.import_('pandas as pd')
m.import_('numpy as np')
m.import_('seaborn as sns')
m.sep()

m.stmt('"# [jupyter][matplotlib][python] color mapの一覧をheatmapで"')
m.stmt('# %matplotlib inline')
with m.with_('code()'):
    m.stmt('xs = np.arange(1, 10)')
    m.stmt('ys = np.arange(1, 10).reshape(9, 1)')
    m.stmt('m = xs * ys')
    m.stmt('df = pd.DataFrame(m)')
    m.stmt('df')

for name in cm.cmap_d.keys():
    m.stmt(f'"## {name}"')
    m.sep()
    with m.with_("code()"):
        m.stmt(f"sns.heatmap(df, {name!r})")
    m.sep()

print(m)
