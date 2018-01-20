package main

import (
	"encoding/json"
	"fmt"
	"log"
)

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

type defaultDispatacher struct {
	slackNotifier *slackNotifier
}

func (d *defaultDispatacher) dispatchForX(status string) {
	d.slackNotifier.notifyForX(status)
}

type dispatcher interface {
	dispatchForX(status string)
}

func newDispatcher(dest string) dispatcher {
	return &defaultDispatacher{slackNotifier: newSlackNotifier(dest)}
}

func doSomething(name string) (string, error) {
	fmt.Println("  do something for", name)
	return "ok", nil
}

func doSomething2(result string) error {
	fmt.Println("  do something2 for", result)
	return nil
}

func runX(d dispatcher) error {
	result, err := doSomething("X")
	if err != nil {
		return err
	}
	d.dispatchForX(result)
	if err := doSomething2("X"); err != nil {
		return err
	}
	return nil
}

type app struct {
	dispatcher dispatcher
}

func (app *app) run(call bool) error {
	fmt.Println("*")
	if call {
		runX(app.dispatcher)
	}
	return nil
}

type config struct {
	Slack struct {
		Destination string `json:"destination"`
	} `json:"slack"`
}

func main() {
	// 実際のアプリでは何か設定ファイルに書かれている
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

	app := makeApp(c)
	call := true
	app.run(call)
}

func makeApp(c config) *app {
	return &app{
		dispatcher: newDispatcher(c.Slack.Destination),
	}
}
