package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/dustin/go-humanize"
)

func getPathOrExit(code int) string {
	if len(os.Args) < 2 {
		fmt.Println("Must provide path")
		os.Exit(code)
	}
	return os.Args[1]
}

// Only for *nix systems for now
func isHidden(shortPath string) bool {
	return shortPath[0] == '.'
}

func onWalked(root string) filepath.WalkFunc {
	return func(path string, info os.FileInfo, err error) error {
		if err != nil {
			panic(err)
		}
		shortPath := strings.Replace(path, root, "", 1)
		if shortPath == "" || isHidden(shortPath) {
			return nil
		} else {
			level := getLevel(shortPath)
			fmt.Printf("[%10s] %s [%3d] %s\n", humanize.Bytes(uint64(info.Size())), emoji(info.IsDir()), level, shortPath)
			return nil
		}
	}
}

func emoji(isDir bool) string {
	if isDir {
		return "📁"
	} else {
		return "📝"
	}
}

func getLevel(shortPath string) int {
	return len(strings.Split(shortPath, string(filepath.Separator))) - 1
}

/*
echo /main.go | entr -cr go run .//main.go ~/code/explore/elm/turtle/
go build -o ./dist/tree_util ./main.go
*/
func main() {
	root := ensureTrailingSlash(getPathOrExit(1))
	err := filepath.Walk(root, onWalked(root))
	if err != nil {
		fmt.Println(err)
	}
}

func ensureTrailingSlash(dirPath string) string {
	separator := string(filepath.Separator)
	if strings.HasSuffix(dirPath, separator) {
		return dirPath
	} else {
		return dirPath + separator
	}
}
