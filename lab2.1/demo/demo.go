package main

import "fmt"

type User struct {
	Name string `json:"name"`
}

var globalMsg = "Global message"

func main() {
	localMsg := "Local message"
	fmt.Println("Hello, World!")

	const greeting = "Welcome!"

	fmt.Println(getMessage("User"))

	words := []string{"Go", "is", "awesome"}
	fmt.Println(words, localMsg)

	translations := map[string]string{
		"hello": "привет",
		"world": "мир",
	}
	fmt.Println(translations)

	user := User{Name: "Alice"}
	fmt.Println(user)
}

func getMessage(name string) string {
	return "Hello, " + name
}
