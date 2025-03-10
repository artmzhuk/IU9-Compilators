package main

import (
	"fmt"

	"lab1.3/analyzer"
)

func main() {
	input := "0110b25\n58  78  5214b\n\t?52141 011b\n ```bipki```"
	scanner := analyzer.Scanner{
		Program: input,
		Position: analyzer.Position{
			Line:  1,
			Pos:   1,
			Index: 0,
			Text:  []rune(input),
		},
		Errors: analyzer.MessageList{
			Messages: make([]analyzer.Message, 0),
		},
	}
	t := scanner.NextToken()
	for t != nil {
		fmt.Println(t)
		t = scanner.NextToken()
	}
	errors := scanner.Errors.GetErrors()
	for _, v := range errors {
		fmt.Println(v)
	}
}
