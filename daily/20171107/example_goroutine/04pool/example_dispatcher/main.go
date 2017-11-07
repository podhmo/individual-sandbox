package main

import (
	"fmt"
	"log"
	"sync"
	"time"
)

// Dispatcher represents a management workers.
type Dispatcher struct {
	pool    chan *worker     // Idle 状態の worker の受け入れ先
	queue   chan interface{} // メッセージの受け入れ先
	workers []*worker
	wg      sync.WaitGroup // 非同期処理の待機用
	quit    chan struct{}
}

// Start starts the specified dispatcher but does not wait for it to complete.
func (d *Dispatcher) Start() {
	for _, w := range d.workers {
		w.start()
	}

	go func() {
		for {
			select {
			// メッセージがキューイングされた場合、 v にメッセージを設定
			case v := <-d.queue:
				(<-d.pool).data <- v // 下記の2行と同じ意味
				// worker := <-d.pool // d.pool から Idle の worker がpoolingされるまで待機
				// worker.data <- v // worker.data に メッセージ v を送信

			case <-d.quit:
				return
			}
		}
	}()
}

// Add adds a given value to the queue of the dispatcher.
func (d *Dispatcher) Add(v interface{}) {
	// キューイングされた場合に処理を待機するために WaitGroup をカウントアップ
	d.wg.Add(1)
	d.queue <- v
}

// Wait waits for the dispatcher to exit. It must have been started by Start.
func (d *Dispatcher) Wait() {
	d.wg.Wait()
}

// worker represents the worker that executes the job.
type worker struct {
	dispatcher *Dispatcher
	data       chan interface{} // 受け取ったメッセージの受信先
	quit       chan struct{}
}

func (w *worker) start() {
	go func() {
		for {
			// dispatcher の pool に自身を送信する（Idle状態を示す）
			w.dispatcher.pool <- w

			select {
			// メッセージがキューイングされた場合、 v にメッセージを設定
			case v := <-w.data:
				if str, ok := v.(string); ok {
					// get 関数でHTTPリクエスト
					log.Printf("get %s\n", str)
					time.Sleep(1 * time.Second)
					log.Printf("got %s\n", str)
				}

				// WaitGroupのカウントダウン
				w.dispatcher.wg.Done()

			case <-w.quit:
				return
			}
		}
	}()
}

const (
	maxWorkers = 3
	maxQueues  = 10000
)

// NewDispatcher returns a pointer of Dispatcher.
func NewDispatcher() *Dispatcher {
	// dispatcher の初期化
	d := &Dispatcher{
		pool:  make(chan *worker, maxWorkers),    // capacity は用意する worker の数
		queue: make(chan interface{}, maxQueues), // capacity はメッセージをキューイングする数
		quit:  make(chan struct{}),
	}

	// worker の初期化
	d.workers = make([]*worker, cap(d.pool))
	for i := 0; i < cap(d.pool); i++ {
		w := worker{
			dispatcher: d,
			data:       make(chan interface{}), // worker でキューイングする場合は capacity を2以上
			quit:       make(chan struct{}),
		}
		d.workers[i] = &w
	}
	return d
}

func main() {
	d := NewDispatcher()

	d.Start()
	for i := 0; i < 100; i++ {
		url := fmt.Sprintf("http://placehold.it/%dx%d", i, i)
		d.Add(url)
	}
	d.Wait()
}
