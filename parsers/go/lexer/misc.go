package lexer

func must(err error) {
	if err != nil {
		panic(err)
	}
}

func must2(_ any, err error) {
	if err != nil {
		panic(err)
	}
}
