import time
import tqdm

with tqdm.tqdm(total=100) as pbar:
    for i in range(10):
        pbar.update(10)
        time.sleep(0.1)
