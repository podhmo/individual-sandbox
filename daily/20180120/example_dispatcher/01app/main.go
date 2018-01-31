package main

import (
	"encoding/json"
	"fmt"
	"log"
)

func doSomething(name string) (string, error) {
	fmt.Println("  do something for", name)
	return "ok", nil
}
func doSomething2(name string) error {
	fmt.Println("  do something for", name)
	return nil
}

type slackNotifier struct {
	destination string // 通知先が複数の場合にはもう少し凝った構成にする
}

func newSlackNotifier(dest string) *slackNotifier {
	return &slackNotifier{destination: dest}
}

func (n *slackNotifier) notifyForX(status string) {
	fmt.Println("----------------------------------------")
	fmt.Printf("slack (%s)\n", n.destination)
	fmt.Println("...", status)
	fmt.Println("----------------------------------------")
}

func runX(n *slackNotifier) error {
	result, err := doSomething("X")
	if err != nil {
		return err
	}
	n.notifyForX(result)
	if err := doSomething2("X"); err != nil {
		return err
	}
	return nil
}

func main() {
	type config struct {
		Slack struct {
			Destination string `json:"destination"`
		} `json:"slack"`
	}

	var c config
	s := `
{
  "slack": {
    "destination": "#dev-x-notif"
  }
}
`
	if err := json.Unmarshal([]byte(s), &c); err != nil {
		log.Fatal(err)
	}

	slackNotifier := newSlackNotifier(c.Slack.Destination)
	runX(slackNotifier)
}
