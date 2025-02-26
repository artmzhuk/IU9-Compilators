package main

import "regexp"

type Tokenizer struct {
	input       string
	row         int
	position    int
	rawPosition int
	domains     []*regexp.Regexp
}

func newTokenizer(regex []string) Tokenizer {
	tokenizer = Tokenizer{}
}

func main() {

}
