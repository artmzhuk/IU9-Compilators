package main

import (
	"fmt"

	"lab1.3/analyzer"
)

func main() {
	input := "0110b25\n58  78  5214b\n\t?52141 011b\n ```bipki```\n ахахха `lol`"
	scanner := analyzer.NewScanner(input)
	t := scanner.NextToken()
	for t != nil {
		fmt.Println(t)
		t = scanner.NextToken()
	}
	errors := scanner.GetErrors()
	for _, v := range errors {
		fmt.Println(v)
	}
}
