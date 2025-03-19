package analyzer

import (
	"strconv"
)

type Automata struct {
	TransitionTable [][]int
	IsFinalState    func(c int) TokenInt
	GetSymbolIdx    func(symbol rune) int
	currentState    int
	prevFinalState  int
}

func (a *Automata) GetCurrentState() int {
	return a.currentState
}

func (a *Automata) SwitchToState(newState int) {
	if newState > len(a.TransitionTable) {
		panic("wrong state jump")
	}
	a.currentState = newState
}

func (a *Automata) GetNextState(symbol rune) int {
	id := a.GetSymbolIdx(symbol)
	if id != -1 {
		return a.TransitionTable[a.currentState][id]
	} else {
		return -1
	}
}

type AutomataScanner struct {
	position     position
	errors       messageList
	Names        NameDictionary
	automata     Automata
	comments     []Fragment
	prevPosition position
}

func NewAutomataScanner(input string, a Automata) *AutomataScanner {
	s := &AutomataScanner{
		position: position{
			Line:        1,
			Pos:         1,
			Index:       0,
			Text:        []rune(input),
			currentRune: []rune(input)[0],
			//reader: bufio.NewReader(strings.NewReader(input)),
		},
		errors: messageList{
			Messages: make([]Message, 0),
		},
		Names: NameDictionary{
			counter:  0,
			IndexMap: make(map[int]string),
			NamesMap: make(map[string]int),
		},
		automata: a,
	}
	//s.position.readRune()
	return s
}

func (s *AutomataScanner) NextToken() *token {
	for s.position.cp() != -1 {
		if s.position.isWhiteSpace() {
			for s.position.isWhiteSpace() {
				s.position.next()
			}
			continue
		}
		currentFragment := Fragment{
			starting: s.position,
			ending:   position{},
		}
		tokenRunes := make([]rune, 0)
		s.automata.SwitchToState(0)
		if s.automata.GetNextState(s.position.cp()) != -1 && s.position.cp() != -1 {
			for s.automata.currentState != -1 {
				if s.position.cp() != '\r' {
					tokenRunes = append(tokenRunes, s.position.cp())
				} else {
					tokenRunes = append(tokenRunes, '\n')
				}
				if s.automata.IsFinalState(s.automata.GetCurrentState()) != -1 {
					s.automata.prevFinalState = s.automata.currentState
					s.prevPosition = s.position
				}
				s.automata.SwitchToState(s.automata.GetNextState(s.position.cp()))
				if s.automata.currentState != -1 {
					s.position.next()
				}
			}
			s.automata.currentState = s.automata.prevFinalState
			s.position = s.prevPosition
			if s.automata.IsFinalState(s.automata.GetCurrentState()) != -1 {
				currentTokenId := s.automata.IsFinalState(s.automata.GetCurrentState())
				currentFragment.ending = s.position
				identTokenID := ""
				if currentTokenId == IDENT_TOKEN {
					identIndex := -1
					if s.Names.Contains(string(tokenRunes)) {
						identIndex = s.Names.GetCode(string(tokenRunes))
					} else {
						identIndex = s.Names.AddName(string(tokenRunes))
					}
					identTokenID = " id" + strconv.Itoa(identIndex)
				} else if currentTokenId == COMMENT_TOKEN {
					if len(tokenRunes) >= 4 { // а меньше и быть не может (**)
						currentFragment.comments = string(tokenRunes[2 : len(tokenRunes)-2])
					}
					s.comments = append(s.comments, currentFragment)
					continue
				}
				return &token{
					coords:    currentFragment,
					domainTag: currentTokenId,
					value:     string(tokenRunes[:len(tokenRunes)-1]) + identTokenID,
				}
			} else {
				// остановились не в финальном состоянии
				s.errors.addError(s.position, "Неизвестный токен")
				for s.automata.GetNextState(s.position.cp()) == -1 && s.position.cp() != -1 {
					s.position.next()
				}
				continue
			}
		} else {
			s.errors.addError(s.position, "Неизвестный токен")
			for s.automata.GetNextState(s.position.cp()) == -1 && s.position.cp() != -1 {
				s.position.next()
			}
			continue
		}
	}
	return nil
}

func (s *AutomataScanner) GetErrors() []Message {
	return s.errors.Messages
}

func (s *AutomataScanner) GetComments() []Fragment {
	return s.comments
}
