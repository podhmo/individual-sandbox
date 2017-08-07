import numpy as np
from matplotlib import pyplot as plt
from nbreversible import code
plt.style.use("ggplot")

# % matplotlib inline


def sample(N, sample_size=100000):
    X_n = np.random.gumbel(1.5, 3.0, [sample_size, N])
    return X_n[:, :].mean(1)


def show_gaussian_fit(sample, bins):
    mu = sample.mean()
    sigma = sample.std()
    a = sample.size * (bins[1] - bins[0])
    plt.hold(True)
    plt.plot(
        bins, a / (np.sqrt(2 * np.pi) * sigma) * np.exp(-(bins - mu) ** 2 / (2 * sigma ** 2)), 'b-'
    )

with code():
    S_N = sample(20)
    bins = np.linspace(-5, 15, 100)
    plt.hist(S_N, bins)
    show_gaussian_fit(S_N, bins)
    plt.gca().axes.get_yaxis().set_ticks([])
    # turn off y ticks
