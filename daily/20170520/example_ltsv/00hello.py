from mylogger import basicConfig, ltsvrender
from hello import hello
from broken import broken


def main():
    import logging
    print("----")
    print("@", hello())
    print("----")
    # basicConfig(level=logging.INFO, render=ltsvrender)
    basicConfig(level=logging.INFO)
    print("@", hello())
    print("----")
    print("@", broken())
    print("----")


if __name__ == "__main__":
    main()
