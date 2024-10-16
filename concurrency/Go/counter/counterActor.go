package counter

const (
	actionInc = iota
	actionGet
)

type payload struct {
	action int
	gotInt chan int
}

type actor struct {
	requests chan payload
}

func NewCounterActor() *actor {
	newActor := &actor{
		requests: make(chan payload),
	}

	go func() {
		state := 0
		for req := range newActor.requests {
			switch req.action {
			case actionInc:
				state++
			case actionGet:
				req.gotInt <- state
			}
		}
	}()

	return newActor
}

func (s *actor) Inc() {
	s.requests <- payload{
		action: actionInc,
	}
}

func (s *actor) GetVal() int {
	retChan := make(chan int)
	s.requests <- payload{
		action: actionGet,
		gotInt: retChan,
	}
	return <-retChan
}
