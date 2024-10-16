package main

import (
	"counter/counter"
	"fmt"
	"sync"
)

/*
go run ./main.go
*/
func main() {
	counter := &counter.Counter{}

	var wg sync.WaitGroup

	for i := 0; i < 3; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 100000; j++ {
				counter.Inc()
			}
		}()
	}

	wg.Wait()

	fmt.Println("Counter is:", counter.GetVal())
}
