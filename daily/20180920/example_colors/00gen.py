from nbreversible import code
import pandas as pd
import numpy as np
import seaborn as sns
# %matplotlib inline

"""# [jupyter][matplotlib][python] color mapの一覧をheatmapで"""

with code():
    xs = np.arange(1, 10)
    ys = np.arange(1, 10).reshape(9, 1)
    m = xs * ys
    df = pd.DataFrame(m)
    df

"""## Blues"""


with code():
    sns.heatmap(df, cmap='Blues')


"""## BrBG"""


with code():
    sns.heatmap(df, cmap='BrBG')


"""## BuGn"""


with code():
    sns.heatmap(df, cmap='BuGn')


"""## BuPu"""


with code():
    sns.heatmap(df, cmap='BuPu')


"""## CMRmap"""


with code():
    sns.heatmap(df, cmap='CMRmap')


"""## GnBu"""


with code():
    sns.heatmap(df, cmap='GnBu')


"""## Greens"""


with code():
    sns.heatmap(df, cmap='Greens')


"""## Greys"""


with code():
    sns.heatmap(df, cmap='Greys')


"""## OrRd"""


with code():
    sns.heatmap(df, cmap='OrRd')


"""## Oranges"""


with code():
    sns.heatmap(df, cmap='Oranges')


"""## PRGn"""


with code():
    sns.heatmap(df, cmap='PRGn')


"""## PiYG"""


with code():
    sns.heatmap(df, cmap='PiYG')


"""## PuBu"""


with code():
    sns.heatmap(df, cmap='PuBu')


"""## PuBuGn"""


with code():
    sns.heatmap(df, cmap='PuBuGn')


"""## PuOr"""


with code():
    sns.heatmap(df, cmap='PuOr')


"""## PuRd"""


with code():
    sns.heatmap(df, cmap='PuRd')


"""## Purples"""


with code():
    sns.heatmap(df, cmap='Purples')


"""## RdBu"""


with code():
    sns.heatmap(df, cmap='RdBu')


"""## RdGy"""


with code():
    sns.heatmap(df, cmap='RdGy')


"""## RdPu"""


with code():
    sns.heatmap(df, cmap='RdPu')


"""## RdYlBu"""


with code():
    sns.heatmap(df, cmap='RdYlBu')


"""## RdYlGn"""


with code():
    sns.heatmap(df, cmap='RdYlGn')


"""## Reds"""


with code():
    sns.heatmap(df, cmap='Reds')


"""## Spectral"""


with code():
    sns.heatmap(df, cmap='Spectral')


"""## Wistia"""


with code():
    sns.heatmap(df, cmap='Wistia')


"""## YlGn"""


with code():
    sns.heatmap(df, cmap='YlGn')


"""## YlGnBu"""


with code():
    sns.heatmap(df, cmap='YlGnBu')


"""## YlOrBr"""


with code():
    sns.heatmap(df, cmap='YlOrBr')


"""## YlOrRd"""


with code():
    sns.heatmap(df, cmap='YlOrRd')


"""## afmhot"""


with code():
    sns.heatmap(df, cmap='afmhot')


"""## autumn"""


with code():
    sns.heatmap(df, cmap='autumn')


"""## binary"""


with code():
    sns.heatmap(df, cmap='binary')


"""## bone"""


with code():
    sns.heatmap(df, cmap='bone')


"""## brg"""


with code():
    sns.heatmap(df, cmap='brg')


"""## bwr"""


with code():
    sns.heatmap(df, cmap='bwr')


"""## cool"""


with code():
    sns.heatmap(df, cmap='cool')


"""## coolwarm"""


with code():
    sns.heatmap(df, cmap='coolwarm')


"""## copper"""


with code():
    sns.heatmap(df, cmap='copper')


"""## cubehelix"""


with code():
    sns.heatmap(df, cmap='cubehelix')


"""## flag"""


with code():
    sns.heatmap(df, cmap='flag')


"""## gist_earth"""


with code():
    sns.heatmap(df, cmap='gist_earth')


"""## gist_gray"""


with code():
    sns.heatmap(df, cmap='gist_gray')


"""## gist_heat"""


with code():
    sns.heatmap(df, cmap='gist_heat')


"""## gist_ncar"""


with code():
    sns.heatmap(df, cmap='gist_ncar')


"""## gist_rainbow"""


with code():
    sns.heatmap(df, cmap='gist_rainbow')


"""## gist_stern"""


with code():
    sns.heatmap(df, cmap='gist_stern')


"""## gist_yarg"""


with code():
    sns.heatmap(df, cmap='gist_yarg')


"""## gnuplot"""


with code():
    sns.heatmap(df, cmap='gnuplot')


"""## gnuplot2"""


with code():
    sns.heatmap(df, cmap='gnuplot2')


"""## gray"""


with code():
    sns.heatmap(df, cmap='gray')


"""## hot"""


with code():
    sns.heatmap(df, cmap='hot')


"""## hsv"""


with code():
    sns.heatmap(df, cmap='hsv')


"""## jet"""


with code():
    sns.heatmap(df, cmap='jet')


"""## nipy_spectral"""


with code():
    sns.heatmap(df, cmap='nipy_spectral')


"""## ocean"""


with code():
    sns.heatmap(df, cmap='ocean')


"""## pink"""


with code():
    sns.heatmap(df, cmap='pink')


"""## prism"""


with code():
    sns.heatmap(df, cmap='prism')


"""## rainbow"""


with code():
    sns.heatmap(df, cmap='rainbow')


"""## seismic"""


with code():
    sns.heatmap(df, cmap='seismic')


"""## spring"""


with code():
    sns.heatmap(df, cmap='spring')


"""## summer"""


with code():
    sns.heatmap(df, cmap='summer')


"""## terrain"""


with code():
    sns.heatmap(df, cmap='terrain')


"""## winter"""


with code():
    sns.heatmap(df, cmap='winter')


"""## Accent"""


with code():
    sns.heatmap(df, cmap='Accent')


"""## Dark2"""


with code():
    sns.heatmap(df, cmap='Dark2')


"""## Paired"""


with code():
    sns.heatmap(df, cmap='Paired')


"""## Pastel1"""


with code():
    sns.heatmap(df, cmap='Pastel1')


"""## Pastel2"""


with code():
    sns.heatmap(df, cmap='Pastel2')


"""## Set1"""


with code():
    sns.heatmap(df, cmap='Set1')


"""## Set2"""


with code():
    sns.heatmap(df, cmap='Set2')


"""## Set3"""


with code():
    sns.heatmap(df, cmap='Set3')


"""## tab10"""


with code():
    sns.heatmap(df, cmap='tab10')


"""## tab20"""


with code():
    sns.heatmap(df, cmap='tab20')


"""## tab20b"""


with code():
    sns.heatmap(df, cmap='tab20b')


"""## tab20c"""


with code():
    sns.heatmap(df, cmap='tab20c')


"""## Blues_r"""


with code():
    sns.heatmap(df, cmap='Blues_r')


"""## BrBG_r"""


with code():
    sns.heatmap(df, cmap='BrBG_r')


"""## BuGn_r"""


with code():
    sns.heatmap(df, cmap='BuGn_r')


"""## BuPu_r"""


with code():
    sns.heatmap(df, cmap='BuPu_r')


"""## CMRmap_r"""


with code():
    sns.heatmap(df, cmap='CMRmap_r')


"""## GnBu_r"""


with code():
    sns.heatmap(df, cmap='GnBu_r')


"""## Greens_r"""


with code():
    sns.heatmap(df, cmap='Greens_r')


"""## Greys_r"""


with code():
    sns.heatmap(df, cmap='Greys_r')


"""## OrRd_r"""


with code():
    sns.heatmap(df, cmap='OrRd_r')


"""## Oranges_r"""


with code():
    sns.heatmap(df, cmap='Oranges_r')


"""## PRGn_r"""


with code():
    sns.heatmap(df, cmap='PRGn_r')


"""## PiYG_r"""


with code():
    sns.heatmap(df, cmap='PiYG_r')


"""## PuBu_r"""


with code():
    sns.heatmap(df, cmap='PuBu_r')


"""## PuBuGn_r"""


with code():
    sns.heatmap(df, cmap='PuBuGn_r')


"""## PuOr_r"""


with code():
    sns.heatmap(df, cmap='PuOr_r')


"""## PuRd_r"""


with code():
    sns.heatmap(df, cmap='PuRd_r')


"""## Purples_r"""


with code():
    sns.heatmap(df, cmap='Purples_r')


"""## RdBu_r"""


with code():
    sns.heatmap(df, cmap='RdBu_r')


"""## RdGy_r"""


with code():
    sns.heatmap(df, cmap='RdGy_r')


"""## RdPu_r"""


with code():
    sns.heatmap(df, cmap='RdPu_r')


"""## RdYlBu_r"""


with code():
    sns.heatmap(df, cmap='RdYlBu_r')


"""## RdYlGn_r"""


with code():
    sns.heatmap(df, cmap='RdYlGn_r')


"""## Reds_r"""


with code():
    sns.heatmap(df, cmap='Reds_r')


"""## Spectral_r"""


with code():
    sns.heatmap(df, cmap='Spectral_r')


"""## Wistia_r"""


with code():
    sns.heatmap(df, cmap='Wistia_r')


"""## YlGn_r"""


with code():
    sns.heatmap(df, cmap='YlGn_r')


"""## YlGnBu_r"""


with code():
    sns.heatmap(df, cmap='YlGnBu_r')


"""## YlOrBr_r"""


with code():
    sns.heatmap(df, cmap='YlOrBr_r')


"""## YlOrRd_r"""


with code():
    sns.heatmap(df, cmap='YlOrRd_r')


"""## afmhot_r"""


with code():
    sns.heatmap(df, cmap='afmhot_r')


"""## autumn_r"""


with code():
    sns.heatmap(df, cmap='autumn_r')


"""## binary_r"""


with code():
    sns.heatmap(df, cmap='binary_r')


"""## bone_r"""


with code():
    sns.heatmap(df, cmap='bone_r')


"""## brg_r"""


with code():
    sns.heatmap(df, cmap='brg_r')


"""## bwr_r"""


with code():
    sns.heatmap(df, cmap='bwr_r')


"""## cool_r"""


with code():
    sns.heatmap(df, cmap='cool_r')


"""## coolwarm_r"""


with code():
    sns.heatmap(df, cmap='coolwarm_r')


"""## copper_r"""


with code():
    sns.heatmap(df, cmap='copper_r')


"""## cubehelix_r"""


with code():
    sns.heatmap(df, cmap='cubehelix_r')


"""## flag_r"""


with code():
    sns.heatmap(df, cmap='flag_r')


"""## gist_earth_r"""


with code():
    sns.heatmap(df, cmap='gist_earth_r')


"""## gist_gray_r"""


with code():
    sns.heatmap(df, cmap='gist_gray_r')


"""## gist_heat_r"""


with code():
    sns.heatmap(df, cmap='gist_heat_r')


"""## gist_ncar_r"""


with code():
    sns.heatmap(df, cmap='gist_ncar_r')


"""## gist_rainbow_r"""


with code():
    sns.heatmap(df, cmap='gist_rainbow_r')


"""## gist_stern_r"""


with code():
    sns.heatmap(df, cmap='gist_stern_r')


"""## gist_yarg_r"""


with code():
    sns.heatmap(df, cmap='gist_yarg_r')


"""## gnuplot_r"""


with code():
    sns.heatmap(df, cmap='gnuplot_r')


"""## gnuplot2_r"""


with code():
    sns.heatmap(df, cmap='gnuplot2_r')


"""## gray_r"""


with code():
    sns.heatmap(df, cmap='gray_r')


"""## hot_r"""


with code():
    sns.heatmap(df, cmap='hot_r')


"""## hsv_r"""


with code():
    sns.heatmap(df, cmap='hsv_r')


"""## jet_r"""


with code():
    sns.heatmap(df, cmap='jet_r')


"""## nipy_spectral_r"""


with code():
    sns.heatmap(df, cmap='nipy_spectral_r')


"""## ocean_r"""


with code():
    sns.heatmap(df, cmap='ocean_r')


"""## pink_r"""


with code():
    sns.heatmap(df, cmap='pink_r')


"""## prism_r"""


with code():
    sns.heatmap(df, cmap='prism_r')


"""## rainbow_r"""


with code():
    sns.heatmap(df, cmap='rainbow_r')


"""## seismic_r"""


with code():
    sns.heatmap(df, cmap='seismic_r')


"""## spring_r"""


with code():
    sns.heatmap(df, cmap='spring_r')


"""## summer_r"""


with code():
    sns.heatmap(df, cmap='summer_r')


"""## terrain_r"""


with code():
    sns.heatmap(df, cmap='terrain_r')


"""## winter_r"""


with code():
    sns.heatmap(df, cmap='winter_r')


"""## Accent_r"""


with code():
    sns.heatmap(df, cmap='Accent_r')


"""## Dark2_r"""


with code():
    sns.heatmap(df, cmap='Dark2_r')


"""## Paired_r"""


with code():
    sns.heatmap(df, cmap='Paired_r')


"""## Pastel1_r"""


with code():
    sns.heatmap(df, cmap='Pastel1_r')


"""## Pastel2_r"""


with code():
    sns.heatmap(df, cmap='Pastel2_r')


"""## Set1_r"""


with code():
    sns.heatmap(df, cmap='Set1_r')


"""## Set2_r"""


with code():
    sns.heatmap(df, cmap='Set2_r')


"""## Set3_r"""


with code():
    sns.heatmap(df, cmap='Set3_r')


"""## tab10_r"""


with code():
    sns.heatmap(df, cmap='tab10_r')


"""## tab20_r"""


with code():
    sns.heatmap(df, cmap='tab20_r')


"""## tab20b_r"""


with code():
    sns.heatmap(df, cmap='tab20b_r')


"""## tab20c_r"""


with code():
    sns.heatmap(df, cmap='tab20c_r')


"""## magma"""


with code():
    sns.heatmap(df, cmap='magma')


"""## magma_r"""


with code():
    sns.heatmap(df, cmap='magma_r')


"""## inferno"""


with code():
    sns.heatmap(df, cmap='inferno')


"""## inferno_r"""


with code():
    sns.heatmap(df, cmap='inferno_r')


"""## plasma"""


with code():
    sns.heatmap(df, cmap='plasma')


"""## plasma_r"""


with code():
    sns.heatmap(df, cmap='plasma_r')


"""## viridis"""


with code():
    sns.heatmap(df, cmap='viridis')


"""## viridis_r"""


with code():
    sns.heatmap(df, cmap='viridis_r')


"""## cividis"""


with code():
    sns.heatmap(df, cmap='cividis')


"""## cividis_r"""


with code():
    sns.heatmap(df, cmap='cividis_r')
