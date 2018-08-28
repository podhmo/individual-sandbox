from nbreversible import code
# %matplotlib inline
import matplotlib.pyplot as plt
from sklearn import datasets
from sklearn.manifold import TSNE


with code():
    digits = datasets.load_digits()

    print(digits.data.shape)
    # (1797, 64)

    print(digits.target.shape)
    # (1797,)

    X_reduced = TSNE(n_components=2, random_state=0).fit_transform(digits.data)

    print(X_reduced.shape)
    # (1797, 2)

    plt.scatter(X_reduced[:, 0], X_reduced[:, 1], c=digits.target)
    plt.colorbar()
