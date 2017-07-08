import matplotlib
matplotlib.use('Agg')  # NOQA
import matplotlib.pyplot as plt
plt.style.use('ggplot')

plt.plot([1, 2, 3, 4])
plt.ylabel('some numbers')

plt.savefig("images/line.png", format="png")
plt.savefig("images/line.svg", format="svg")
plt.savefig("images/line.ps", format="ps")
plt.savefig("images/line.pdf", format="pdf")
