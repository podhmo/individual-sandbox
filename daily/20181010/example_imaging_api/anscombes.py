import seaborn as sns
import matplotlib.pyplot as plt

sns.set(style="ticks")

# Load the example dataset for Anscombe's quartet
df = sns.load_dataset("anscombe")
print(df)
# Show the results of a linear regression within each dataset
ax = sns.lmplot(
    x="x",
    y="y",
    col="dataset",
    hue="dataset",
    data=df,
    col_wrap=2,
    ci=None,
    palette="muted",
    height=4,
    scatter_kws={
        "s": 50,
        "alpha": 1
    }
)
plt.show()
