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

func doCountLines(r *bufio.Reader) int {
	n := 0
	for {
		_, err := r.ReadString('\n')
		if err != nil {
			break
		}
		n++
	}
	return n
}

func doCountWords(r *bufio.Reader) int {
	n := 0
	inWord := false
	for {
		b, err := r.ReadByte()
		if err != nil {
			break
		}
		isWhiteSpace := b == ' ' || b == '\r' || b == '\n' || b == '\t'
		if inWord {
			if isWhiteSpace {
				inWord = false
			}
		} else {
			if !isWhiteSpace {
				inWord = true
				n++
			}
		}
	}
	return n
}

func doCountRunes(r *bufio.Reader) int {
	n := 0
	for {
		_, _, err := r.ReadRune()
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
	countWords := flag.Bool("w", false, "count words")
	countRunes := flag.Bool("m", false, "count runes")
	flag.Parse()

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
		if *countBytes {
			gotBytesCount := doCountBytes(reader)
			fmt.Printf("%d %s\n", gotBytesCount, baseName)
		} else if *countLines {
			gotLinesCount := doCountLines(reader)
			fmt.Printf("%d %s\n", gotLinesCount, baseName)
		} else if *countWords {
			gotWordsCount := doCountWords(reader)
			fmt.Printf("%d %s\n", gotWordsCount, baseName)
		} else if *countRunes {
			gotRunesCount := doCountRunes(reader)
			fmt.Printf("%d %s\n", gotRunesCount, baseName)
		}

	}

}
