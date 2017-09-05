import time
import tqdm


class TqdmUpTo(tqdm.tqdm):
    def update_to(self, b):
        self.update(b)


with TqdmUpTo(total=20) as pbar:
    for i in range(20):
        pbar.update_to(i)
        time.sleep(0.1)
