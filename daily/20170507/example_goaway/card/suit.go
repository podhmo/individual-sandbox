package card

import (
	"fmt"
)

// Cardsuit : 記号
type Cardsuit string

const (
	// CardsuitSpadeds : スペード
	CardsuitSpadeds = Cardsuit("♠")
	// CardsuitHearts : ハート
	CardsuitHearts = Cardsuit("♥")
	// CardsuitDiamonds : ダイヤ
	CardsuitDiamonds = Cardsuit("♦")
	// CardsuitClubs : クラブ
	CardsuitClubs = Cardsuit("♣")
)

// String : stringer implementation
func (c Cardsuit) String() string {
	switch c {
	case CardsuitSpadeds:
		return "spadeds"
	case CardsuitHearts:
		return "hearts"
	case CardsuitDiamonds:
		return "diamonds"
	case CardsuitClubs:
		return "clubs"
	default:
		panic(fmt.Sprintf("unexpected Cardsuit %s, in string()", string(c)))
	}

}
// ParseCardsuit : parse
func ParseCardsuit(c string) Cardsuit {
	switch c {
	case "♠":
		return CardsuitSpadeds
	case "♥":
		return CardsuitHearts
	case "♦":
		return CardsuitDiamonds
	case "♣":
		return CardsuitClubs
	default:
		panic(fmt.Sprintf("unexpected Cardsuit %v, in parse()", c))
	}

}