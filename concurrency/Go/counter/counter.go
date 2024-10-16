package counter

import "sync"

type Counter struct {
	val  int
	lock sync.Mutex
}

func (t *Counter) Inc() {
	t.lock.Lock()
	t.val++
	t.lock.Unlock()
}

func (t *Counter) GetVal() int {
	t.lock.Lock()
	defer t.lock.Unlock()
	return t.val
}
