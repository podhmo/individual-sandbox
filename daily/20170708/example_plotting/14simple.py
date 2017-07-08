import matplotlib
matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
plt.style.use('ggplot')

plt.plot([1, 2, 3, 4])
plt.ylabel('y')

dpi = float(plt.gcf().get_dpi())
plt.gcf().set_size_inches(400 / dpi, 300 / dpi)

plt.savefig("images/line-400x300.pdf", dpi=dpi)
