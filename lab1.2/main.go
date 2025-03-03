package main

import (
	"fmt"
	"os"
	"regexp"
	"unicode/utf8"
)

type Pair struct {
	first  string // название токена
	second string // исходная строка регулярки
}

type Token struct {
	name, tType, matched string
	line, column         int
	regex                *regexp.Regexp
}

func (t Token) String() string {
	res := fmt.Sprintf("%s (%d, %d): %s", t.name, t.line, t.column, t.matched)
	return res
}

type Tokenizer struct {
	input              string
	line               int
	charPositionInLine int
	bytePosition       int
	domains            []Token
}

func (t *Tokenizer) AdvanceCharacter() rune {
	if t.input[t.bytePosition] == '\n' {
		t.bytePosition++
		t.charPositionInLine = 0
		t.line++
		return '\n'
	}
	char, runeLen := utf8.DecodeRune([]byte(t.input[t.bytePosition:]))
	t.bytePosition += runeLen
	t.charPositionInLine++
	return char
}

func (t *Tokenizer) SkipWhitespaces() {
	for t.bytePosition < len(t.input) {
		if t.input[t.bytePosition] != '\n' && t.input[t.bytePosition] != '\t' &&
			t.input[t.bytePosition] != ' ' && t.input[t.bytePosition] != '\r' {
			break
		}
		t.AdvanceCharacter()
	}
}

func (t *Tokenizer) NextToken() *Token {
	isInErrorState := false
	t.SkipWhitespaces()
	for t.bytePosition < len(t.input) {
		for _, domain := range t.domains {
			if res := domain.regex.FindStringIndex(t.input[t.bytePosition:]); res != nil {
				isInErrorState = false
				token := &Token{
					name:    domain.name,
					line:    t.line,
					column:  t.charPositionInLine,
					matched: t.input[t.bytePosition+res[0] : t.bytePosition+res[1]],
				}
				for i := 0; i < utf8.RuneCountInString(token.matched); i++ {
					t.AdvanceCharacter()
				}
				return token
			}
		}
		if !isInErrorState {
			isInErrorState = true
			fmt.Printf("syntax error (%d, %d)\n", t.line, t.charPositionInLine)
		}
		t.AdvanceCharacter()
	}
	return nil
}

func newTokenizer(input string, regex []Pair) (Tokenizer, error) {
	tokenizer := Tokenizer{
		input:              input,
		line:               1,
		charPositionInLine: 0,
		bytePosition:       0,
		domains:            make([]Token, 0, len(regex)),
	}
	for _, v := range regex {
		currentRegex, err := regexp.Compile(v.second)
		if err != nil {
			return Tokenizer{}, err
		}
		tokenizer.domains = append(tokenizer.domains, Token{
			name:  v.first,
			regex: currentRegex,
		})
	}
	return tokenizer, nil
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
	input := openFile("lab1.2/test.txt")
	//input := `"Unfinished string`
	t, err := newTokenizer(input, []Pair{{
		first:  "IDENT1",
		second: `^\A"(?:\\.|[^"\\])*"`,
	}, {
		first:  "NUMBER",
		second: `^(0|1+)`,
	}, {
		first:  "IDENT2",
		second: `\A@"(?:[^"]|"")*"`,
	},
	})
	if err != nil {
		fmt.Println(err)
		return
	}
	curr := t.NextToken()
	for curr != nil {
		fmt.Println(curr)
		curr = t.NextToken()
	}
}
