package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strconv"
)

func findLiterals(file *ast.File) []string {
	// Вызываем обход дерева, начиная от корня
	literals := make([]string, 0)
	ast.Inspect(file, func(node ast.Node) bool {
		if _, ok := node.(*ast.ImportSpec); ok {
			return false
		}

		if basicStmt, ok := node.(*ast.BasicLit); ok {
			if basicStmt.Kind == token.STRING {
				literals = append(literals, basicStmt.Value)
			}
		}
		return true
	})
	return literals
}

func addConstants(file *ast.File, literals []string) *ast.File {
	added := make(map[string]int)
	counter := 0
	for _, v := range literals {
		curr := 0
		if added[v] != 0 {
			curr = added[v]
		} else {
			curr = counter
		}
		file.Decls = append(file.Decls,
			&ast.GenDecl{
				Tok: token.CONST,
				Specs: []ast.Spec{
					&ast.ValueSpec{
						Names: []*ast.Ident{&ast.Ident{
							Name: "const" + strconv.Itoa(counter),
							Obj:  ast.NewObj(ast.ObjKind(ast.Con), "const"+strconv.Itoa(curr)),
						}},
						Values: []ast.Expr{
							&ast.BasicLit{
								Kind:  token.STRING,
								Value: v,
							},
						},
					},
				},
			},
		)
		if added[v] == 0 {
			added[v] = counter
			counter++
		}
	}
	return file
}

func replace(file *ast.File) []string {
	literals := make([]string, 0)
	ast.Inspect(file, func(node ast.Node) bool {
		if _, ok := node.(*ast.ValueSpec); ok {

			return false
		}
	})
	return literals
}

func findRequiredChild() {

}

func main() {

	// Создаём хранилище данных об исходных файлах
	fset := token.NewFileSet()

	// Вызываем парсер
	if file, err := parser.ParseFile(
		fset,                 // данные об исходниках
		"demo.go",            // имя файла с исходником программы
		nil,                  // пусть парсер сам загрузит исходник
		parser.ParseComments, // приказываем сохранять комментарии
	); err == nil {
		// Если парсер отработал без ошибок, печатаем дерево
		res := findLiterals(file)
		file = addConstants(file, res)

		ast.Fprint(os.Stdout, fset, file, nil)
		//format.Node(os.Stdout, fset, file)
		fmt.Println(res)
	} else {
		// в противном случае, выводим сообщение об ошибке
		fmt.Printf("Error: %v", err)
	}
}
