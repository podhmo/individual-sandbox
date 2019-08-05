package message

type Message string

func NewMessage(phrase string) Message {
	return Message(phrase)
}
