package analyzer

import (
	"fmt"
)

const (
	STR_TOKEN = iota
	NUMBER_TOKEN
	BINARY_TOKEN
	IDENT_TOKEN
)

type Message struct {
	Coord position
	Text  string
}

func (m Message) String() string {
	return fmt.Sprintf("%s %s", m.Coord.String(), m.Text)
}

type token struct {
	coords    fragment
	domainTag int
	tag       string
	value     any
}

func (t token) String() string {
	return fmt.Sprintf("%s %s: %v", t.tag, t.coords.String(), t.value)
}

type messageList struct {
	Messages []Message
}

func (m *messageList) addError(coord position, text string) {
	m.Messages = append(m.Messages, Message{
		Coord: coord,
		Text:  text,
	})
}

func (m *messageList) getErrors() []Message {
	return m.Messages
}

/*
func (m *MessageList) AddWarning(coord position, text string) {

}

func (m *MessageList) GetSorted() []Message {
	return nil
}

type NameDictionary struct {
	counter  int
	NamesMap map[string]int
	NamesArr []string
}

func (d *NameDictionary) AddName(text string) int {
	currIndex := d.counter
	d.NamesMap[text] = currIndex
	d.NamesArr = append(d.NamesArr, text)
	if currIndex != len(d.NamesArr) {
		panic("achtung!")
	}
	d.counter++
	return currIndex
}
func (d *NameDictionary) Contains(text string) bool {
	_, ok := d.NamesMap[text]
	return ok
}

func (d *NameDictionary) GetName(code int) string {
	if code >= len(d.NamesArr) {
		panic("achtung 2")
	}
	return d.NamesArr[code]
}

type Compiler struct {
	Names    NameDictionary
	Messages MessageList
}*/
