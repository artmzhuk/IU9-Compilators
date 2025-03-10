package analyzer

import (
	"fmt"
	"strconv"
)

type Position struct {
	Line, Pos, Index int
	Text             []rune
}

func (p *Position) Cp() rune {
	if p.Index == len(p.Text) {
		return -1
	} else {
		return p.Text[p.Index]
	}
}

func (p *Position) IsNewLine() bool {
	if p.Index == len(p.Text) {
		return true
	}
	if p.Text[p.Index] == '\r' && p.Index+1 < len(p.Text) {
		return p.Text[p.Index+1] == '\n'
	}
	return p.Text[p.Index] == '\n'
}

func (p *Position) IsWhiteSpace() bool {
	if p.IsNewLine() {
		return true
	}
	if p.Index != len(p.Text) && (p.Text[p.Index] == ' ' || p.Text[p.Index] == '\t') {
		return true
	}
	return false
}

func (p *Position) IsDigit() bool {
	return p.Cp() >= '0' && p.Cp() <= '9'
}

func (p *Position) Next() {
	if p.Index < len(p.Text) {
		if p.IsNewLine() {
			if p.Text[p.Index] == '\r' {
				p.Index++
			}
			p.Line++
			p.Pos = 0
		}
		p.Pos++
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

func (f Fragment) String() string {
	return f.Starting.String() + "-" + f.Ending.String()
}

type Message struct {
	Coord Position
	Text  string
}

func (m Message) String() string {
	return fmt.Sprintf("%s %s", m.Coord.String(), m.Text)
}

const (
	STR_TOKEN = iota
	NUMBER_TOKEN
	BINARY_TOKEN
	IDENT_TOKEN
)

type Token struct {
	Coords    Fragment
	DomainTag int
	Tag       string
	Value     any
}

func (t Token) String() string {
	return fmt.Sprintf("%s %s: %v", t.Tag, t.Coords.String(), t.Value)
}

type Scanner struct {
	Program  string
	Position Position
	Errors   MessageList
}

func (s *Scanner) NextToken() *Token {
	for s.Position.Cp() != -1 {
		for s.Position.IsWhiteSpace() {
			s.Position.Next()
		}
		currentFragment := Fragment{
			Starting: s.Position,
			Ending:   Position{},
		}
		tokenRunes := make([]rune, 0)
		if s.Position.Cp() == '`' {
			s.Position.Next()
			for s.Position.Cp() != -1 {
				if s.Position.Cp() == '`' {
					s.Position.Next()
					if s.Position.Cp() != '`' { //кавычка не продублировалась, значит она закрывающая
						currentFragment.Ending = s.Position
						return &Token{
							Coords:    currentFragment,
							DomainTag: STR_TOKEN,
							Tag:       "STR_TOKEN",
							Value:     string(tokenRunes),
						}
					}
				}
				tokenRunes = append(tokenRunes, s.Position.Cp())
				s.Position.Next()
			}
			s.Errors.AddError(s.Position, "Отсутствует закрывающая кавычка")
			continue
		} else if s.Position.IsDigit() {
			for s.Position.IsDigit() {
				tokenRunes = append(tokenRunes, s.Position.Cp())
				s.Position.Next()
			}
			var tokenValue int64
			var err error
			if s.Position.Cp() == 'b' {
				s.Position.Next()
				currentFragment.Ending = s.Position
				tokenValue, err = strconv.ParseInt(string(tokenRunes), 2, 64)
				if err != nil {
					s.Errors.AddError(s.Position, "Число не двоичное, но присутствует b")
					continue
				}
				return &Token{
					Coords:    currentFragment,
					DomainTag: BINARY_TOKEN,
					Tag:       "BINARY_TOKEN",
					Value:     tokenValue,
				}
			} else {
				currentFragment.Ending = s.Position
				tokenValue, err = strconv.ParseInt(string(tokenRunes), 10, 64)
				if err != nil {
					panic(err) // паникуем, тк символы проверены ранее условием '0' <= x <= '9'
				}
				return &Token{
					Coords:    currentFragment,
					DomainTag: NUMBER_TOKEN,
					Tag:       "NUMBER_TOKEN",
					Value:     tokenValue,
				}
			}
		} else if s.Position.Cp() == '?' || s.Position.Cp() == '*' || s.Position.Cp() == '|' {
			for s.Position.IsDigit() || s.Position.Cp() == '?' || s.Position.Cp() == '*' || s.Position.Cp() == '|' {
				tokenRunes = append(tokenRunes, s.Position.Cp())
				s.Position.Next()
			}
			currentFragment.Ending = s.Position
			return &Token{
				Coords:    currentFragment,
				DomainTag: IDENT_TOKEN,
				Tag:       "IDENT_TOKEN",
				Value:     string(tokenRunes),
			}
		} else {
			// hz
		}
		return nil
	}
	return nil
}

type MessageList struct {
	Messages []Message
}

func (m *MessageList) AddError(coord Position, text string) {
	m.Messages = append(m.Messages, Message{
		Coord: coord,
		Text:  text,
	})
}

func (m *MessageList) GetErrors() []Message {
	return m.Messages
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
