package analyzer

import (
	"fmt"
)

const (
	STR_TOKEN = iota
	NUMBER_TOKEN
	BINARY_TOKEN
	IDENT_TOKEN
	COMMENT_TOKEN
	CASE_TOKEN
	BREAK_TOKEN
	LBR_TOKEN
	STAR_TOKEN
	RBR_TOKEN
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
	return fmt.Sprintf("%v %s: %v", t.domainTag, t.coords.String(), t.value)
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

type NameDictionary struct {
	counter  int
	NamesMap map[string]int
	IndexMap map[int]string
}

func (d *NameDictionary) AddName(text string) int {
	d.NamesMap[text] = d.counter
	d.IndexMap[d.counter] = text
	d.counter++
	return d.counter - 1
}
func (d *NameDictionary) Contains(text string) bool {
	_, ok := d.NamesMap[text]
	return ok
}

func (d *NameDictionary) GetName(code int) string {
	return d.IndexMap[code]
}

func (d *NameDictionary) GetCode(name string) int {
	return d.NamesMap[name]
}

// type Compiler struct {
// 	Names    NameDictionary
// 	Messages []Message
// }
