package analyzer

import (
	"fmt"
)

type position struct {
	currentRune      rune
	Line, Pos, Index int
	//reader           *bufio.Reader
	Text []rune
}

func (p *position) cp() rune {
	if p.Index == len(p.Text) {
		return -1
	}
	return p.currentRune
}

func (p *position) isNewLine() bool {
	if p.cp() == -1 {
		return true
	}
	if p.cp() == '\r' {
		return true
		// p.readRune()
		// peek := p.cp()
		// err := p.reader.UnreadRune()
		// if err != nil {
		// 	panic("achtung 3")
		// }
		// p.currentRune = '\r'
		// return peek == '\n'
	}
	return p.cp() == '\n'
}

func (p *position) isWhiteSpace() bool {
	if p.cp() == -1 {
		return false
	}
	if p.isNewLine() || p.cp() == ' ' || p.cp() == '\t' {
		return true
	}
	return false
}

func (p *position) isDigit() bool {
	return p.cp() >= '0' && p.cp() <= '9'
}

func (p *position) readRune() {
	// readRune, _, err := p.reader.ReadRune()
	// if err != nil {
	// 	if errors.Is(err, io.EOF) {
	// 		p.currentRune = -1
	// 		return
	// 	}
	// 	fmt.Println(err)
	// 	p.currentRune = 0xFFFD
	// 	return
	// }
	p.Index++
	if p.Index >= len(p.Text) {
		p.currentRune = -1
		return
	}
	p.currentRune = p.Text[p.Index]
	return
}

func (p *position) next() {
	if p.isNewLine() {
		if p.cp() == '\r' {
			p.readRune()
		}
		p.Line++
		p.Pos = 0
	}
	p.readRune()
	p.Pos++
}
func (p *position) String() string {
	return fmt.Sprintf("(%d, %d)", p.Line, p.Pos)
}

type Fragment struct {
	starting position
	ending   position
	comments string
}

func (f Fragment) String() string {
	return f.starting.String() + "-" + f.ending.String() + " comments: " + f.comments
}
