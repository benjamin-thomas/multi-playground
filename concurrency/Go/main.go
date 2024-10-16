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
	cnt1 := &counter.Counter{}
	cnt2 := counter.NewCounterActor()

	var wg sync.WaitGroup

	for i := 0; i < 3; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 100000; j++ {
				cnt1.Inc()
				cnt2.Inc()
			}
		}()
	}

	wg.Wait()

	fmt.Println("Counter1 is:", cnt1.GetVal())
	fmt.Println("Counter2 is:", cnt2.GetVal())

}
