package solution

import (
	"strconv"
	"strings"
	"testing"
)

// echo ./solution_test.go | entr -c go test

func interleaveLetters(wordA string, wordB string) string {
	charsA := []rune(wordA)
	charsB := []rune(wordB)

	var sb strings.Builder

	longest := max(len(charsA), len(charsB))

	for i := 0; i < longest; i++ {
		if i < len(charsA) {
			sb.WriteRune(charsA[i])
		}
		if i < len(charsB) {
			sb.WriteRune(charsB[i])
		}
	}
	return sb.String()

}

func TestInterleaveLetters(t *testing.T) {
	type testCase = struct {
		wordA string
		wordB string
		want  string
	}
	testCases := []testCase{
		{wordA: "AC", wordB: "BD", want: "ABCD"},
		{wordA: "AC_EFG", wordB: "BD", want: "ABCD_EFG"},
		{wordA: "AC", wordB: "BD_EFG", want: "ABCD_EFG"},
		{wordA: "ABC", wordB: "", want: "ABC"},
		{wordA: "", wordB: "ABC", want: "ABC"},
		{wordA: "", wordB: "", want: ""},
	}
	for i, tc := range testCases {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			got := interleaveLetters(tc.wordA, tc.wordB)
			if got != tc.want {
				t.Errorf("interleaveLetters() = %q, want %q", got, tc.want)
			}
		})

	}

}
