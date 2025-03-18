package main

import (
	"fmt"
	"os"

	"lab1.4/analyzer"
)

func openFile(name string) string {
	read, err := os.ReadFile(name)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	return string(read)
}

func main() {
	/*	if len(os.Args) > 1 {
			input := openFile(os.Args[1])
			scanner = analyzer.NewStringScanner(input)
		} else {
			scanner = analyzer.NewStdinScanner()
		}*/
	i := openFile("test.txt")
	scanner := analyzer.NewAutomataScanner(i)
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
