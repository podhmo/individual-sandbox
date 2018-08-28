from nbreversible import (
    code
)
# %matplotlib inline
import matplotlib.pyplot as plt
from sklearn import datasets
from sklearn.decomposition import PCA


with code():
    digits = datasets.load_digits()

    print(digits.data.shape)
    # (1797, 64)

    print(digits.target.shape)
    # (1797,)

    X_reduced = PCA(n_components=2).fit_transform(digits.data)

    print(X_reduced.shape)
    # (1797, 2)

    plt.scatter(X_reduced[:, 0], X_reduced[:, 1], c=digits.target)
    plt.colorbar()
