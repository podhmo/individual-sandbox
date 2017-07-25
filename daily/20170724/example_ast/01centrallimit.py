from hmm import cell

with cell("code"):
    # %matplotlib inline
    import numpy as np
    from scipy import stats
    import matplotlib.pyplot as plt


@cell("code")
def central_limit_plot(ax, rvs, title=""):
    mean = np.mean(rvs, axis=0)
    ax.hist(mean, bins=20, normed=True)
    ax.set_title(title)


with cell("code"):
    M = 10000

# this sentence treated at markdown text

with cell("code"):
    # uniform
    rvs_1 = stats.uniform.rvs(size=(1, M))
    rvs_5 = stats.uniform.rvs(size=(5, M))
    rvs_10 = stats.uniform.rvs(size=(10, M))

    fig, axes = plt.subplots(1, 3, figsize=(10, 5))
    central_limit_plot(axes[0], rvs_1, title="$N=1$")
    central_limit_plot(axes[1], rvs_5, title="$N=5$")
    central_limit_plot(axes[2], rvs_10, title="$N=10$")
    fig.suptitle("uniform", fontsize=18)

with cell("code"):
    # binomial
    n = 100
    p = 0.3
    rvs_1 = stats.binom.rvs(n, p, size=(1, M))
    rvs_5 = stats.binom.rvs(n, p, size=(5, M))
    rvs_10 = stats.binom.rvs(n, p, size=(10, M))

    fig, axes = plt.subplots(1, 3, figsize=(10, 5))
    central_limit_plot(axes[0], rvs_1, title="$N=1$")
    central_limit_plot(axes[1], rvs_5, title="$N=5$")
    central_limit_plot(axes[2], rvs_10, title="$N=10$")
    fig.suptitle("binomial", fontsize=18)

with cell("code"):
    # poisson
    mu = 2.5
    rvs_1 = stats.poisson.rvs(mu, size=(1, M))
    rvs_5 = stats.poisson.rvs(mu, size=(5, M))
    rvs_10 = stats.poisson.rvs(mu, size=(10, M))

    fig, axes = plt.subplots(1, 3, figsize=(10, 5))
    central_limit_plot(axes[0], rvs_1, title="$N=1$")
    central_limit_plot(axes[1], rvs_5, title="$N=5$")
    central_limit_plot(axes[2], rvs_10, title="$N=10$")
    fig.suptitle("poisson", fontsize=18)
