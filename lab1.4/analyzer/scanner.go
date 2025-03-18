package analyzer

type Scanner interface {
	NextToken() *token
	GetErrors() []Message
	GetComments() []Fragment
}
