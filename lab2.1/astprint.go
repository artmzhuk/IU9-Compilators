package main

import (
	"fmt"
	"go/ast"
	"go/format"
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
		if _, ok := node.(*ast.Field); ok {
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

func addConstants(file *ast.File, literals []string) (*ast.File, map[string]int) {
	addedConstantsIdxs := make(map[string]int)
	constCounter := 1
	startOfConsts := 0
	for i := range file.Decls {
		if stmt, ok := file.Decls[i].(*ast.GenDecl); ok && stmt.Tok == token.IMPORT {
			startOfConsts++
			continue
		}
		break
	} // вычисляем где начать вставку констант

	for _, v := range literals {
		if addedConstantsIdxs[v] != 0 {
			continue
		}
		file.Decls = append(file.Decls[:startOfConsts+1], file.Decls[startOfConsts:]...)
		file.Decls[startOfConsts] = &ast.GenDecl{
			Tok: token.CONST,
			Specs: []ast.Spec{
				&ast.ValueSpec{
					Names: []*ast.Ident{{
						Name: "const" + strconv.Itoa(constCounter),
					}},
					Values: []ast.Expr{
						&ast.BasicLit{
							Kind:  token.STRING,
							Value: v,
						},
					},
				},
			},
		}
		addedConstantsIdxs[v] = constCounter
		startOfConsts++
		constCounter++
	}
	return file, addedConstantsIdxs
}

func createIdentNode(name string, names map[string]int) *ast.Ident {
	constName := "const" + strconv.Itoa(names[name])
	ident := &ast.Ident{
		Name: constName,
	}
	return ident
}

func replace(file *ast.File, names map[string]int) *ast.File {
	for _, v := range file.Decls {
		if _, ok := v.(*ast.FuncDecl); ok {
			ast.Inspect(v, func(node ast.Node) bool {
				switch castedNode := node.(type) {
				case *ast.CallExpr:
					for i := range castedNode.Args {
						if bl, ok := castedNode.Args[i].(*ast.BasicLit); ok && bl.Kind == token.STRING {
							castedNode.Args[i] = createIdentNode(bl.Value, names)
						}
					}
				case *ast.BinaryExpr:
					if bl, ok := castedNode.X.(*ast.BasicLit); ok {
						castedNode.X = createIdentNode(bl.Value, names)
					}
					if bl, ok := castedNode.Y.(*ast.BasicLit); ok {
						castedNode.Y = createIdentNode(bl.Value, names)
					}
				case *ast.ValueSpec:
					for i := range castedNode.Values {
						if bl, ok := castedNode.Values[i].(*ast.BasicLit); ok && bl.Kind == token.STRING {
							castedNode.Values[i] = createIdentNode(bl.Value, names)
						}
					}
				case *ast.AssignStmt:
					for i, v := range castedNode.Rhs {
						if bl, ok := v.(*ast.BasicLit); ok {
							castedNode.Rhs[i] = createIdentNode(bl.Value, names)
						}
					}
				case *ast.CompositeLit:
					for i, v := range castedNode.Elts {
						if bl, ok := v.(*ast.BasicLit); ok {
							castedNode.Elts[i] = createIdentNode(bl.Value, names)
						}
					}
				case *ast.KeyValueExpr:
					if bl, ok := castedNode.Key.(*ast.BasicLit); ok {
						castedNode.Key = createIdentNode(bl.Value, names)
					}
					if bl, ok := castedNode.Value.(*ast.BasicLit); ok {
						castedNode.Value = createIdentNode(bl.Value, names)
					}
				}
				return true
			})
		}
	}
	return file
}

func main() {
	fset := token.NewFileSet()
	astTree, err := parser.ParseFile(
		fset,                 // данные об исходниках
		"demo/demo.go",       // имя файла с исходником программы
		nil,                  // пусть парсер сам загрузит исходник
		parser.ParseComments, // приказываем сохранять комментарии
	)
	if err != nil {
		fmt.Printf("Error: %v", err)
		return
	}
	strLiterals := findLiterals(astTree)
	astTree, namesMap := addConstants(astTree, strLiterals)

	resFile, err := os.Create("res.txt")
	if err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	astTree = replace(astTree, namesMap)

	ast.Fprint(resFile, fset, astTree, nil)
	resFile2, err := os.Create("res/res2.go")
	if err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	format.Node(resFile2, fset, astTree)
}
