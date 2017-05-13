from goaway import get_repository


def define(package):
    f = package.file("suit.go")
    suit = f.enum("Cardsuit", f.string, comment="記号")
    suit.define_member("spadeds", "♠", comment="スペード")
    suit.define_member("hearts", "♥", comment="ハート")
    suit.define_member("diamonds", "♦", comment="ダイヤ")
    suit.define_member("clubs", "♣", comment="クラブ")

    f = package.file("card.go")
    card = f.struct("Card", comment="カード")
    card.define_field("Suit", type=suit)
    card.define_field("Value", type=f.int8)


def main():
    r = get_repository()
    package = r.package("card")
    define(package)
    import logging
    logging.basicConfig(level=logging.INFO)
    r.emitter.emit_package(package, d="./card")


if __name__ == "__main__":
    main()
