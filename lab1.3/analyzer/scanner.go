package analyzer

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Scanner struct {
	position position
	errors   messageList
	Names    NameDictionary
}

func NewStdinScanner() Scanner {
	s := Scanner{
		position: position{
			Line:   1,
			Pos:    1,
			Index:  0,
			reader: bufio.NewReader(os.Stdin),
		},
		errors: messageList{
			Messages: make([]Message, 0),
		},
		Names: NameDictionary{
			counter:  0,
			IndexMap: make(map[int]string),
			NamesMap: make(map[string]int),
		},
	}
	s.position.readRune()
	return s
}

func NewStringScanner(input string) Scanner {
	s := Scanner{
		position: position{
			Line:   1,
			Pos:    1,
			Index:  0,
			reader: bufio.NewReader(strings.NewReader(input)),
			//Text:  []rune(input),
		},
		errors: messageList{
			Messages: make([]Message, 0),
		},
		Names: NameDictionary{
			counter:  0,
			IndexMap: make(map[int]string),
			NamesMap: make(map[string]int),
		},
	}
	s.position.readRune()
	return s
}

func (s *Scanner) NextToken() *token {
	for s.position.cp() != -1 {
		if s.position.isWhiteSpace() {
			for s.position.isWhiteSpace() {
				s.position.next()
			}
			continue
		}
		currentFragment := fragment{
			starting: s.position,
			ending:   position{},
		}
		tokenRunes := make([]rune, 0)
		if s.position.cp() == '`' {
			s.position.next()
			for s.position.cp() != -1 {
				if s.position.cp() == '`' {
					s.position.next()
					if s.position.cp() != '`' { //кавычка не продублировалась, значит она закрывающая
						currentFragment.ending = s.position
						return &token{
							coords:    currentFragment,
							domainTag: STR_TOKEN,
							tag:       "STR_TOKEN",
							value:     string(tokenRunes),
						}
					}
				}
				tokenRunes = append(tokenRunes, s.position.cp())
				s.position.next()
			}
			s.errors.addError(s.position, "Отсутствует закрывающая кавычка")
			continue
		} else if s.position.isDigit() {
			for s.position.isDigit() {
				tokenRunes = append(tokenRunes, s.position.cp())
				s.position.next()
			}
			var tokenValue int64
			var err error
			if s.position.cp() == 'b' {
				s.position.next()
				currentFragment.ending = s.position
				tokenValue, err = strconv.ParseInt(string(tokenRunes), 2, 64)
				if err != nil {
					s.errors.addError(s.position, "Число не двоичное, но присутствует b")
					continue
				}
				return &token{
					coords:    currentFragment,
					domainTag: BINARY_TOKEN,
					tag:       "BINARY_TOKEN",
					value:     tokenValue,
				}
			} else {
				currentFragment.ending = s.position
				tokenValue, err = strconv.ParseInt(string(tokenRunes), 10, 64)
				if err != nil {
					panic(err) // паникуем, тк символы проверены ранее условием '0' <= x <= '9'
				}
				return &token{
					coords:    currentFragment,
					domainTag: NUMBER_TOKEN,
					tag:       "NUMBER_TOKEN",
					value:     tokenValue,
				}
			}
		} else if s.position.cp() == '?' || s.position.cp() == '*' || s.position.cp() == '|' {
			for s.position.isDigit() || s.position.cp() == '?' || s.position.cp() == '*' || s.position.cp() == '|' {
				tokenRunes = append(tokenRunes, s.position.cp())
				s.position.next()
			}
			currentFragment.ending = s.position
			identIndex := -1
			if s.Names.Contains(string(tokenRunes)) {
				identIndex = s.Names.GetCode(string(tokenRunes))
			} else {
				identIndex = s.Names.AddName(string(tokenRunes))
			}
			return &token{
				coords:    currentFragment,
				domainTag: IDENT_TOKEN,
				tag:       "IDENT_TOKEN",
				value:     fmt.Sprintf("%s: index %d", string(tokenRunes), identIndex),
			}
		} else {
			s.errors.addError(s.position, "Неизвестный токен")
			for !s.position.isDigit() && s.position.cp() != '?' && s.position.cp() != '*' && s.position.cp() != '|' &&
				s.position.cp() != '`' && s.position.cp() != -1 {
				s.position.next()
			}
			//fmt.Println("achtung")
			continue
		}
		return nil
	}
	return nil
}

func (s *Scanner) GetErrors() []Message {
	return s.errors.Messages
}
