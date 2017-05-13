from goaway import get_repository


def define(f):
    suit = f.enum("Cardsuit", f.string, comment="記号")
    suit.define_member("spadeds", "♠", comment="スペード")
    suit.define_member("hearts", "♥", comment="ハート")
    suit.define_member("diamonds", "♦", comment="ダイヤ")
    suit.define_member("clubs", "♣", comment="クラブ")

    card = f.struct("Card", comment="カード")
    card.define_field("Suit", type=suit)
    card.define_field("Value", type=f.int8)


def main():
    r = get_repository()
    f = r.package("card").file("card.go")
    define(f)
    print(r.writer.write(f))


if __name__ == "__main__":
    main()
