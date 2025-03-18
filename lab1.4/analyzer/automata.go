package analyzer

import (
	"bufio"
	"strings"
)

var transitionTable = [][]int{
	/*              0   1   2   3   4   5   6   7   8   9   10  11*/
	/*             Az  09   (   *   )   a   b   c   e   k   r   s    */
	/* state 0 */ {17, 16, 10, 15, 14, 17, 5, 1, 17, 17, 17, 17},
	/* state 1 */ {-1, -1, -1, -1, -1, 2, 17, 17, 17, 17, 17, 17},
	/* state 2 */ {-1, -1, -1, -1, -1, 17, 17, 17, 17, 17, 17, 3},
	/* state 3 */ {-1, -1, -1, -1, -1, 17, 17, 17, 4, 17, 17, 17},
	/* state 4 */ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 5 */ {-1, -1, -1, -1, -1, 17, 17, 17, 17, 17, 6, 17},
	/* state 6 */ {-1, -1, -1, -1, -1, 17, 17, 17, 7, 17, 17, 17},
	/* state 7 */ {-1, -1, -1, -1, -1, 8, 17, 17, 17, 17, 17, 17},
	/* state 8 */ {-1, -1, -1, -1, -1, 17, 17, 17, 17, 9, 17, 17},
	/* state 9 */ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 10*/ {-1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 11*/ {11, 11, 11, 12, 11, 11, 11, 11, 11, 11, 11, 11},
	/* state 12*/ {11, 11, 11, 11, 13, 11, 11, 11, 11, 11, 11, 11},
	/* state 13*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 14*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 15*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 16*/ {-1, 16, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	/* state 17*/ {17, 17, -1, -1, -1, 17, 17, 17, 17, 17, 17, 17},
	/* Az -- все буквы кроме отдельных*/
}

type AutomataScanner struct {
	position     position
	errors       messageList
	Names        NameDictionary
	currentState int
	table        [][]int
}

func NewAutomataScanner(input string) *AutomataScanner {
	s := &AutomataScanner{
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
		currentState: 0,
		table:        transitionTable,
	}
	s.position.readRune()
	return s
}

func (s *AutomataScanner) ChangeState(newState int) {
	s.currentState = newState
}

func (s *AutomataScanner) getAlphabetId(symbol rune) int {
	switch symbol {
	case 'a':
		return 5
	case 'b':
		return 6
	case 'c':
		return 7
	case 'e':
		return 8
	case 'k':
		return 9
	case 'r':
		return 10
	case 's':
		return 11
	case '(':
		return 2
	case '*':
		return 3
	case ')':
		return 4
	}
	if symbol >= '0' && symbol <= '9' {
		return 1
	} else if (symbol >= 'A' && symbol <= 'Z') || (symbol >= 'a' && symbol <= 'z') {
		return 0
	} else {
		return -1
	}
}

func (s *AutomataScanner) NextState(symbol rune) int {
	id := s.getAlphabetId(symbol)
	if id != -1 {
		return s.table[s.currentState][id]
	} else {
		return -1
	}
}

func (s *AutomataScanner) IsFinalState(c int) int {
	if (c >= 1 && c <= 3) || (c >= 5 && c <= 8) || c == 17 {
		return COMMENT_TOKEN
	} else if c == 16 {
		return NUMBER_TOKEN
	} else if c == 4 {
		return CASE_TOKEN
	} else if c == 9 {
		return BREAK_TOKEN
	} else if c == 10 {
		return LBR_TOKEN
	} else if c == 14 {
		return RBR_TOKEN
	} else if c == 15 {
		return STAR_TOKEN
	} else if c == 13 {
		return COMMENT_TOKEN
	}
	return -1
}

func (s *AutomataScanner) NextToken() *token {
	for s.position.cp() != -1 {
		if s.position.isWhiteSpace() {
			for s.position.isWhiteSpace() {
				s.position.next()
			}
			//s.currentState = 0
			continue
		}
		currentFragment := fragment{
			starting: s.position,
			ending:   position{},
		}
		tokenRunes := make([]rune, 0)
		s.currentState = 0
		if s.NextState(s.position.cp()) == -1 && s.position.cp() != -1 {
			s.errors.addError(s.position, "Неизвестный токен")
			for s.NextState(s.position.cp()) == -1 {
				s.position.next()
			}
			continue
		} else {
			for s.NextState(s.position.cp()) != -1 {
				tokenRunes = append(tokenRunes, s.position.cp())
				s.ChangeState(s.NextState(s.position.cp()))
				s.position.next()
			}
			if s.IsFinalState(s.currentState) != -1 {
				currentFragment.ending = s.position
				//s.currentState = 0
				return &token{
					coords:    currentFragment,
					domainTag: s.IsFinalState(s.currentState),
					tag:       "token",
					value:     string(tokenRunes),
				}
			} else {
				s.errors.addError(s.position, "Неизвестный токен")
				for s.NextState(s.position.cp()) == -1 && s.position.cp() != -1 {
					s.position.next()
				}
				continue
			}
		}
	}
	return nil
}

func (s *AutomataScanner) GetErrors() []Message {
	return s.errors.Messages
}
