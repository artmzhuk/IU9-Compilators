package analyzer

import "fmt"

type position struct {
	Line, Pos, Index int
	Text             []rune
}

func (p *position) cp() rune {
	if p.Index == len(p.Text) {
		return -1
	} else {
		return p.Text[p.Index]
	}
}

func (p *position) isNewLine() bool {
	if p.Index == len(p.Text) {
		return true
	}
	if p.Text[p.Index] == '\r' && p.Index+1 < len(p.Text) {
		return p.Text[p.Index+1] == '\n'
	}
	return p.Text[p.Index] == '\n'
}

func (p *position) isWhiteSpace() bool {
	if p.isNewLine() {
		return true
	}
	if p.Index != len(p.Text) && (p.Text[p.Index] == ' ' || p.Text[p.Index] == '\t') {
		return true
	}
	return false
}

func (p *position) isDigit() bool {
	return p.cp() >= '0' && p.cp() <= '9'
}

func (p *position) next() {
	if p.Index < len(p.Text) {
		if p.isNewLine() {
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
func (p *position) String() string {
	return fmt.Sprintf("(%d, %d)", p.Line, p.Pos)
}

type fragment struct {
	starting position
	ending   position
}

func (f fragment) String() string {
	return f.starting.String() + "-" + f.ending.String()
}
