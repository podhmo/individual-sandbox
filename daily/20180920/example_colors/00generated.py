"""
# [jupyter][matplotlib][python] color mapの一覧をheatmapで
"""

from nbreversible import code
import pandas as pd
import numpy as np
import seaborn as sns
# %matplotlib inline


with code():
    xs = np.arange(1, 10)
    ys = np.arange(1, 10).reshape(9, 1)
    m = xs * ys
    df = pd.DataFrame(m)
    df


"""
## Blues
"""

sns.heatmap(df, cmap='Blues')


"""
## BrBG
"""

sns.heatmap(df, cmap='BrBG')


"""
## BuGn
"""

sns.heatmap(df, cmap='BuGn')


"""
## BuPu
"""

sns.heatmap(df, cmap='BuPu')


"""
## CMRmap
"""

sns.heatmap(df, cmap='CMRmap')


"""
## GnBu
"""

sns.heatmap(df, cmap='GnBu')


"""
## Greens
"""

sns.heatmap(df, cmap='Greens')


"""
## Greys
"""

sns.heatmap(df, cmap='Greys')


"""
## OrRd
"""

sns.heatmap(df, cmap='OrRd')


"""
## Oranges
"""

sns.heatmap(df, cmap='Oranges')


"""
## PRGn
"""

sns.heatmap(df, cmap='PRGn')


"""
## PiYG
"""

sns.heatmap(df, cmap='PiYG')


"""
## PuBu
"""

sns.heatmap(df, cmap='PuBu')


"""
## PuBuGn
"""

sns.heatmap(df, cmap='PuBuGn')


"""
## PuOr
"""

sns.heatmap(df, cmap='PuOr')


"""
## PuRd
"""

sns.heatmap(df, cmap='PuRd')


"""
## Purples
"""

sns.heatmap(df, cmap='Purples')


"""
## RdBu
"""

sns.heatmap(df, cmap='RdBu')


"""
## RdGy
"""

sns.heatmap(df, cmap='RdGy')


"""
## RdPu
"""

sns.heatmap(df, cmap='RdPu')


"""
## RdYlBu
"""

sns.heatmap(df, cmap='RdYlBu')


"""
## RdYlGn
"""

sns.heatmap(df, cmap='RdYlGn')


"""
## Reds
"""

sns.heatmap(df, cmap='Reds')


"""
## Spectral
"""

sns.heatmap(df, cmap='Spectral')


"""
## Wistia
"""

sns.heatmap(df, cmap='Wistia')


"""
## YlGn
"""

sns.heatmap(df, cmap='YlGn')


"""
## YlGnBu
"""

sns.heatmap(df, cmap='YlGnBu')


"""
## YlOrBr
"""

sns.heatmap(df, cmap='YlOrBr')


"""
## YlOrRd
"""

sns.heatmap(df, cmap='YlOrRd')


"""
## afmhot
"""

sns.heatmap(df, cmap='afmhot')


"""
## autumn
"""

sns.heatmap(df, cmap='autumn')


"""
## binary
"""

sns.heatmap(df, cmap='binary')


"""
## bone
"""

sns.heatmap(df, cmap='bone')


"""
## brg
"""

sns.heatmap(df, cmap='brg')


"""
## bwr
"""

sns.heatmap(df, cmap='bwr')


"""
## cool
"""

sns.heatmap(df, cmap='cool')


"""
## coolwarm
"""

sns.heatmap(df, cmap='coolwarm')


"""
## copper
"""

sns.heatmap(df, cmap='copper')


"""
## cubehelix
"""

sns.heatmap(df, cmap='cubehelix')


"""
## flag
"""

sns.heatmap(df, cmap='flag')


"""
## gist_earth
"""

sns.heatmap(df, cmap='gist_earth')


"""
## gist_gray
"""

sns.heatmap(df, cmap='gist_gray')


"""
## gist_heat
"""

sns.heatmap(df, cmap='gist_heat')


"""
## gist_ncar
"""

sns.heatmap(df, cmap='gist_ncar')


"""
## gist_rainbow
"""

sns.heatmap(df, cmap='gist_rainbow')


"""
## gist_stern
"""

sns.heatmap(df, cmap='gist_stern')


"""
## gist_yarg
"""

sns.heatmap(df, cmap='gist_yarg')


"""
## gnuplot
"""

sns.heatmap(df, cmap='gnuplot')


"""
## gnuplot2
"""

sns.heatmap(df, cmap='gnuplot2')


"""
## gray
"""

sns.heatmap(df, cmap='gray')


"""
## hot
"""

sns.heatmap(df, cmap='hot')


"""
## hsv
"""

sns.heatmap(df, cmap='hsv')


"""
## jet
"""

sns.heatmap(df, cmap='jet')


"""
## nipy_spectral
"""

sns.heatmap(df, cmap='nipy_spectral')


"""
## ocean
"""

sns.heatmap(df, cmap='ocean')


"""
## pink
"""

sns.heatmap(df, cmap='pink')


"""
## prism
"""

sns.heatmap(df, cmap='prism')


"""
## rainbow
"""

sns.heatmap(df, cmap='rainbow')


"""
## seismic
"""

sns.heatmap(df, cmap='seismic')


"""
## spring
"""

sns.heatmap(df, cmap='spring')


"""
## summer
"""

sns.heatmap(df, cmap='summer')


"""
## terrain
"""

sns.heatmap(df, cmap='terrain')


"""
## winter
"""

sns.heatmap(df, cmap='winter')


"""
## Accent
"""

sns.heatmap(df, cmap='Accent')


"""
## Dark2
"""

sns.heatmap(df, cmap='Dark2')


"""
## Paired
"""

sns.heatmap(df, cmap='Paired')


"""
## Pastel1
"""

sns.heatmap(df, cmap='Pastel1')


"""
## Pastel2
"""

sns.heatmap(df, cmap='Pastel2')


"""
## Set1
"""

sns.heatmap(df, cmap='Set1')


"""
## Set2
"""

sns.heatmap(df, cmap='Set2')


"""
## Set3
"""

sns.heatmap(df, cmap='Set3')


"""
## tab10
"""

sns.heatmap(df, cmap='tab10')


"""
## tab20
"""

sns.heatmap(df, cmap='tab20')


"""
## tab20b
"""

sns.heatmap(df, cmap='tab20b')


"""
## tab20c
"""

sns.heatmap(df, cmap='tab20c')


"""
## Blues_r
"""

sns.heatmap(df, cmap='Blues_r')


"""
## BrBG_r
"""

sns.heatmap(df, cmap='BrBG_r')


"""
## BuGn_r
"""

sns.heatmap(df, cmap='BuGn_r')


"""
## BuPu_r
"""

sns.heatmap(df, cmap='BuPu_r')


"""
## CMRmap_r
"""

sns.heatmap(df, cmap='CMRmap_r')


"""
## GnBu_r
"""

sns.heatmap(df, cmap='GnBu_r')


"""
## Greens_r
"""

sns.heatmap(df, cmap='Greens_r')


"""
## Greys_r
"""

sns.heatmap(df, cmap='Greys_r')


"""
## OrRd_r
"""

sns.heatmap(df, cmap='OrRd_r')


"""
## Oranges_r
"""

sns.heatmap(df, cmap='Oranges_r')


"""
## PRGn_r
"""

sns.heatmap(df, cmap='PRGn_r')


"""
## PiYG_r
"""

sns.heatmap(df, cmap='PiYG_r')


"""
## PuBu_r
"""

sns.heatmap(df, cmap='PuBu_r')


"""
## PuBuGn_r
"""

sns.heatmap(df, cmap='PuBuGn_r')


"""
## PuOr_r
"""

sns.heatmap(df, cmap='PuOr_r')


"""
## PuRd_r
"""

sns.heatmap(df, cmap='PuRd_r')


"""
## Purples_r
"""

sns.heatmap(df, cmap='Purples_r')


"""
## RdBu_r
"""

sns.heatmap(df, cmap='RdBu_r')


"""
## RdGy_r
"""

sns.heatmap(df, cmap='RdGy_r')


"""
## RdPu_r
"""

sns.heatmap(df, cmap='RdPu_r')


"""
## RdYlBu_r
"""

sns.heatmap(df, cmap='RdYlBu_r')


"""
## RdYlGn_r
"""

sns.heatmap(df, cmap='RdYlGn_r')


"""
## Reds_r
"""

sns.heatmap(df, cmap='Reds_r')


"""
## Spectral_r
"""

sns.heatmap(df, cmap='Spectral_r')


"""
## Wistia_r
"""

sns.heatmap(df, cmap='Wistia_r')


"""
## YlGn_r
"""

sns.heatmap(df, cmap='YlGn_r')


"""
## YlGnBu_r
"""

sns.heatmap(df, cmap='YlGnBu_r')


"""
## YlOrBr_r
"""

sns.heatmap(df, cmap='YlOrBr_r')


"""
## YlOrRd_r
"""

sns.heatmap(df, cmap='YlOrRd_r')


"""
## afmhot_r
"""

sns.heatmap(df, cmap='afmhot_r')


"""
## autumn_r
"""

sns.heatmap(df, cmap='autumn_r')


"""
## binary_r
"""

sns.heatmap(df, cmap='binary_r')


"""
## bone_r
"""

sns.heatmap(df, cmap='bone_r')


"""
## brg_r
"""

sns.heatmap(df, cmap='brg_r')


"""
## bwr_r
"""

sns.heatmap(df, cmap='bwr_r')


"""
## cool_r
"""

sns.heatmap(df, cmap='cool_r')


"""
## coolwarm_r
"""

sns.heatmap(df, cmap='coolwarm_r')


"""
## copper_r
"""

sns.heatmap(df, cmap='copper_r')


"""
## cubehelix_r
"""

sns.heatmap(df, cmap='cubehelix_r')


"""
## flag_r
"""

sns.heatmap(df, cmap='flag_r')


"""
## gist_earth_r
"""

sns.heatmap(df, cmap='gist_earth_r')


"""
## gist_gray_r
"""

sns.heatmap(df, cmap='gist_gray_r')


"""
## gist_heat_r
"""

sns.heatmap(df, cmap='gist_heat_r')


"""
## gist_ncar_r
"""

sns.heatmap(df, cmap='gist_ncar_r')


"""
## gist_rainbow_r
"""

sns.heatmap(df, cmap='gist_rainbow_r')


"""
## gist_stern_r
"""

sns.heatmap(df, cmap='gist_stern_r')


"""
## gist_yarg_r
"""

sns.heatmap(df, cmap='gist_yarg_r')


"""
## gnuplot_r
"""

sns.heatmap(df, cmap='gnuplot_r')


"""
## gnuplot2_r
"""

sns.heatmap(df, cmap='gnuplot2_r')


"""
## gray_r
"""

sns.heatmap(df, cmap='gray_r')


"""
## hot_r
"""

sns.heatmap(df, cmap='hot_r')


"""
## hsv_r
"""

sns.heatmap(df, cmap='hsv_r')


"""
## jet_r
"""

sns.heatmap(df, cmap='jet_r')


"""
## nipy_spectral_r
"""

sns.heatmap(df, cmap='nipy_spectral_r')


"""
## ocean_r
"""

sns.heatmap(df, cmap='ocean_r')


"""
## pink_r
"""

sns.heatmap(df, cmap='pink_r')


"""
## prism_r
"""

sns.heatmap(df, cmap='prism_r')


"""
## rainbow_r
"""

sns.heatmap(df, cmap='rainbow_r')


"""
## seismic_r
"""

sns.heatmap(df, cmap='seismic_r')


"""
## spring_r
"""

sns.heatmap(df, cmap='spring_r')


"""
## summer_r
"""

sns.heatmap(df, cmap='summer_r')


"""
## terrain_r
"""

sns.heatmap(df, cmap='terrain_r')


"""
## winter_r
"""

sns.heatmap(df, cmap='winter_r')


"""
## Accent_r
"""

sns.heatmap(df, cmap='Accent_r')


"""
## Dark2_r
"""

sns.heatmap(df, cmap='Dark2_r')


"""
## Paired_r
"""

sns.heatmap(df, cmap='Paired_r')


"""
## Pastel1_r
"""

sns.heatmap(df, cmap='Pastel1_r')


"""
## Pastel2_r
"""

sns.heatmap(df, cmap='Pastel2_r')


"""
## Set1_r
"""

sns.heatmap(df, cmap='Set1_r')


"""
## Set2_r
"""

sns.heatmap(df, cmap='Set2_r')


"""
## Set3_r
"""

sns.heatmap(df, cmap='Set3_r')


"""
## tab10_r
"""

sns.heatmap(df, cmap='tab10_r')


"""
## tab20_r
"""

sns.heatmap(df, cmap='tab20_r')


"""
## tab20b_r
"""

sns.heatmap(df, cmap='tab20b_r')


"""
## tab20c_r
"""

sns.heatmap(df, cmap='tab20c_r')


"""
## magma
"""

sns.heatmap(df, cmap='magma')


"""
## magma_r
"""

sns.heatmap(df, cmap='magma_r')


"""
## inferno
"""

sns.heatmap(df, cmap='inferno')


"""
## inferno_r
"""

sns.heatmap(df, cmap='inferno_r')


"""
## plasma
"""

sns.heatmap(df, cmap='plasma')


"""
## plasma_r
"""

sns.heatmap(df, cmap='plasma_r')


"""
## viridis
"""

sns.heatmap(df, cmap='viridis')


"""
## viridis_r
"""

sns.heatmap(df, cmap='viridis_r')


"""
## cividis
"""

sns.heatmap(df, cmap='cividis')


"""
## cividis_r
"""

sns.heatmap(df, cmap='cividis_r')
