from functools import partial
import itertools
import time
from multiprocessing import Pool


def subcalc(word, collection):
    subdoc = []
    lists = []
    wo = []
    for term in set(word):
        if (collection.tf_idf(term, word) > 0):
            wo.append([term, collection.tf_idf(term, word)])
    wo.sort(key=lambda x: x[1])
    wo.reverse()
    return wo[:20]


def tfidf(word_set, nltk):
    doc = []
    p = Pool()
    collection = nltk.TextCollection(word_set)
    task = partial(subcalc, collection=collection)
    p.map(task, word_set)


if __name__ == "__main__":
    import cProfile
    import nltk
    pr = cProfile.Profile()
    with open("text.txt") as rf:
        word_set = list(itertools.chain.from_iterable([line.rstrip().split(" ") for line in rf]))
    pr.runcall(tfidf, word_set, nltk)
    pr.dump_stats("01concurrent.prof")
