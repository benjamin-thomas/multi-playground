package main

import (
	"fmt"
	"sync"
)

type Counter struct {
	val  int
	lock sync.Mutex
}

func (c *Counter) Inc() {
	c.lock.Lock()
	c.val++
	c.lock.Unlock()
}

func (c *Counter) GetVal() int {
	return c.val
}

/*
go run ./main.go
*/
func main() {
	counter := &Counter{}
	// counter.val = 98 // FIXME: this val is "unprotected"

	// Create a WaitGroup to wait for all goroutines to finish
	var wg sync.WaitGroup

	// Create 3 goroutines
	for i := 0; i < 3; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 100000; j++ {
				counter.Inc()
			}
		}()
	}

	// Wait for all goroutines to finish
	wg.Wait()

	fmt.Println("(Go) Counter is:", counter.val)
}
