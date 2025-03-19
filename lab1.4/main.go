package main

import (
	"fmt"
	"os"

	"lab1.4/analyzer"
)

var transitionTable = [][]int{
	/*              0   1   2   3   4   5   6   7   8   9   10  11  12     */
	/*             Az  09   (   *   )   a   b   c   e   k   r   s    \t\n  */
	/* state 0 */ {17, 16, 10, 15, 14, 17, 5, 1, 17, 17, 17, 17, 0, -1},
	/* state 1 */ {-1, -1, -1, -1, -1, 2, 17, 17, 17, 17, 17, 17, -1, -1},
	/* state 2 */ {-1, -1, -1, -1, -1, 17, 17, 17, 17, 17, 17, 3, -1, -1},
	/* state 3 */ {-1, -1, -1, -1, -1, 17, 17, 17, 4, 17, 17, 17, -1, -1},
	/* state 4 */ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 5 */ {-1, -1, -1, -1, -1, 17, 17, 17, 17, 17, 6, 17, -1, -1},
	/* state 6 */ {-1, -1, -1, -1, -1, 17, 17, 17, 7, 17, 17, 17, -1, -1},
	/* state 7 */ {-1, -1, -1, -1, -1, 8, 17, 17, 17, 17, 17, 17, -1, -1},
	/* state 8 */ {-1, -1, -1, -1, -1, 17, 17, 17, 17, 9, 17, 17, -1, -1},
	/* state 9 */ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 10*/ {-1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 11*/ {11, 11, 11, 12, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11},
	/* state 12*/ {11, 11, 11, 11, 13, 11, 11, 11, 11, 11, 11, 11, 11, 11},
	/* state 13*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 14*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 15*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 16*/ {-1, 16, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 17*/ {17, 17, -1, -1, -1, 17, 17, 17, 17, 17, 17, 17, -1, -1},
	/* Az -- все буквы кроме отдельных*/
}

func getAlphabetId(symbol rune) int {
	switch symbol {
	case 'a':
		return 5
	case 'b':
		return 6
	case 'c':
		return 7
	case 'e':
		return 8
	case 'k':
		return 9
	case 'r':
		return 10
	case 's':
		return 11
	case '(':
		return 2
	case '*':
		return 3
	case ')':
		return 4
	}
	if symbol >= '0' && symbol <= '9' {
		return 1
	} else if (symbol >= 'A' && symbol <= 'Z') || (symbol >= 'a' && symbol <= 'z') {
		return 0
	} else if symbol == '\n' || symbol == '\r' || symbol == '\t' || symbol == ' ' {
		return 12
	} else {
		return 13
	}
}

func IsFinalState(c int) analyzer.TokenInt {
	if (c >= 1 && c <= 3) || (c >= 5 && c <= 8) || c == 17 {
		return analyzer.IDENT_TOKEN
	} else if c == 16 {
		return analyzer.NUMBER_TOKEN
	} else if c == 4 {
		return analyzer.CASE_TOKEN
	} else if c == 9 {
		return analyzer.BREAK_TOKEN
	} else if c == 10 {
		return analyzer.LBR_TOKEN
	} else if c == 14 {
		return analyzer.RBR_TOKEN
	} else if c == 15 {
		return analyzer.STAR_TOKEN
	} else if c == 13 {
		return analyzer.COMMENT_TOKEN
	}
	return -1
}

func openFile(name string) string {
	read, err := os.ReadFile(name)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	return string(read)
}

func main() {
	var scanner analyzer.Scanner
	if len(os.Args) > 0 {
		input := openFile("test.txt")
		scanner = analyzer.NewAutomataScanner(input, analyzer.Automata{
			TransitionTable: transitionTable,
			IsFinalState:    IsFinalState,
			GetSymbolIdx:    getAlphabetId,
		})
	} else {
		fmt.Println("необходим путь к файлу")
		os.Exit(0)
	}
	t := scanner.NextToken()
	for t != nil {
		fmt.Println(t)
		t = scanner.NextToken()
	}
	comments := scanner.GetComments()
	for _, v := range comments {
		fmt.Println(v)
	}
	errors := scanner.GetErrors()
	for _, v := range errors {
		fmt.Println(v)
	}
}
