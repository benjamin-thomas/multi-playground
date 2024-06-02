package main

/*

rg --files | entr -c go run ./main.go -c ../test.txt
rg --files | entr -c go run ./main.go -c ../test.txt /usr/share/dict/words

Want:
  bytes = 342190
  lines = 7145
  words = 58164
  runes = 339292

*/

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"path"
)

func doCountBytes(r *bufio.Reader) int {
	n := 0
	for {
		_, err := r.ReadByte()
		if err != nil {
			break
		}
		n++
	}
	return n
}

func main() {
	countBytes := flag.Bool("c", false, "count bytes")
	countLines := flag.Bool("l", false, "count lines")
	flag.Parse()

	if *countBytes {
		println("Counting bytes")
	} else if *countLines {
		println("Counting lines")
	} else {
		println("other")
	}

	filepaths := flag.Args()

	for _, filepath := range filepaths {
		baseName := path.Base(filepath)
		file, err := os.Open(filepath)
		if err != nil {
			fmt.Printf("%s: %s\n", baseName, err)
			continue
		}
		defer file.Close()

		reader := bufio.NewReader(file)
		gotBytesCount := doCountBytes(reader)
		fmt.Printf("%d %s\n", gotBytesCount, baseName)
	}

}
