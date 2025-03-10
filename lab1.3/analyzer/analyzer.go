package analyzer

import "fmt"

type Position struct {
	Line, Pos, Index int
	text             []rune
}

func (p *Position) Cp() rune {
	if p.Index == len(p.text) {
		return -1
	} else {
		return p.text[p.Index]
	}
}

func (p *Position) IsNewLine() bool {
	if p.Index == len(p.text) {
		return true
	}
	if p.text[p.Index] == '\r' && p.Index+1 < len(p.text) {
		return p.text[p.Index+1] == '\n'
	}
	return p.text[p.Index] == '\n'
}

func (p *Position) IsWhiteSpace() bool {
	if p.IsNewLine() {
		return true
	}
	if p.Index != len(p.text) && (p.text[p.Index] == ' ' || p.text[p.Index] == '\t') {
		return true
	}
	return false
}

func (p *Position) Next() {
	if p.Index < len(p.text) {
		if p.IsNewLine() {
			if p.text[p.Index] == '\r' {
				p.Index++
			}
			p.Line++
			p.Pos = 1
		}
		p.Index++
	}
}
func (p *Position) String() string {
	return fmt.Sprintf("(%d, %d)", p.Line, p.Pos)
}

type Fragment struct {
	Starting Position
	Ending   Position
}

func (f *Fragment) String() string {
	return f.Starting.String() + "_" + f.Ending.String()
}

type Message struct {
	Coord Position
	Error error
	Text  string
}

const (
	STR_TOKEN = iota
	NUMBER_TOKEN
	IDENT_TOKEN
)

type Token struct {
	Coords    Fragment
	DomainTag int
	Tag       string
}

type Scanner struct {
	Program  string
	position Position
}

func (s *Scanner) NextToken() Token {
	for s.position.Cp() != -1 {
		for s.position.IsWhiteSpace() {
			s.position.Next()
		}

	}
	return Token{}
}

type MessageList struct {
	Messages []Message
}

func (m *MessageList) AddError(coord Position, text string) {

}

func (m *MessageList) AddWarning(coord Position, text string) {

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
}
