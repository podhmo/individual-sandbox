from nbreversible import (code, )
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
# %matplotlib inline

with code():
    df = pd.read_csv("./output/scores.tsv", sep="\t")
    df.head()

with code():
    df.describe()

with code():
    cs = sns.color_palette()
    fieldnames = df.columns[2:]
    ax = sns.kdeplot(df[fieldnames[0]], color=cs[0])
    for i, name in enumerate(fieldnames[1:], 1):
        sns.kdeplot(df[name], color=cs[i], ax=ax)

with code():
    fieldnames = df.columns[2:]
    concated = pd.concat(
        [df[[col]].rename(columns={
            col: "value"
        }).assign(name=col) for col in fieldnames]
    )
    sns.boxplot(x=concated["name"], y=concated["value"])

with code():
    sns.violinplot(x=concated["name"], y=concated["value"])

with code():
    sns.boxenplot(x=concated["name"], y=concated["value"])

with code():
    from sklearn import preprocessing  # or scipy.stats.zscore

    fieldnames = df.columns[2:]

    # standardization
    copied_df = pd.DataFrame()
    for name in fieldnames:
        copied_df[name] = preprocessing.scale(df[name].astype("float64"))

    cs = sns.color_palette()
    concated = pd.concat(
        [copied_df[[col]].rename(columns={
            col: "value"
        }).assign(name=col) for col in fieldnames]
    )
    sns.boxenplot(x=concated["name"], y=concated["value"])

with code():
    sns.scatterplot(x="Attack", y="Defense", data=df, alpha=0.4)

with code():
    from sklearn.decomposition import PCA

    data_df = df[df.columns[2:]]
    reduced = PCA(n_components=2).fit_transform(data_df)
    print(reduced.shape)
    plt.scatter(x=reduced[:, 0], y=reduced[:, 1])

with code():
    from sklearn.manifold import TSNE

    data_df = df[df.columns[2:]]
    reduced = TSNE(n_components=2, random_state=0).fit_transform(data_df)
    plt.scatter(reduced[:, 0], reduced[:, 1])
