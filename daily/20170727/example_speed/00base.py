import itertools
import nltk


@profile
def tfidf(word):
    collection = nltk.TextCollection(word)
    doc = []
    for do in word:
        wo = []
        for term in set(do):
            a = collection.tf_idf(term, do)
            if (a > 0):
                wo.append([term, a])
        wo.sort(key=lambda x: x[1])
        wo.reverse()
        slice1 = [i[0] for i in wo]
        lists = slice1[:20]
        doc.append(list(lists))

    return doc


if __name__ == "__main__":
    with open("text.txt") as rf:
        word_set = list(
            itertools.chain.from_iterable([line.rstrip().split(" ") for line in rf])
        )
    tfidf(word_set)
