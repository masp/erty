package ast

type Visitor interface {
	// Visit traverses the AST in depth-first order (every statement is completely traversed before the next).
	// If the visitor returns an error, then the traversal stops and the error is returned to the caller.
	Visit(n Node) error
}

type visitorFunc func(Node) error

func (v visitorFunc) Visit(n Node) error {
	return v(n)
}

func VisitorFunc(fn func(Node) error) Visitor {
	return visitorFunc(fn)
}

type bailout error

// mustVisit panics if the walk fails, expecting to be caught by the recover in Walk.
// This is because writing the err != nil is very repetitive and obscures the actual logic.
func mustVisit(v Visitor, n Node) {
	err := v.Visit(n)
	if err != nil {
		panic(bailout(err))
	}
}

func walk(n Node, v Visitor) {
	mustVisit(v, n) // visit root first

	switch n := n.(type) {
	case *Module:
		walk(n.Id, v)
		for _, decl := range n.Decls {
			walk(decl, v)
		}
	case *ImportDecl:
		if n.Alias != nil {
			walk(n.Alias, v)
		}
		walk(n.Path, v)
	case *FuncDecl:
		walk(n.Name, v)
		for _, param := range n.Parameters.List {
			walk(param, v)
		}
		for _, stmt := range n.Statements {
			walk(stmt, v)
		}
	case *TypeDecl:
		walk(n.Name, v)
		walk(n.Definition, v)
	case *ReturnStatement:
		walk(n.Expression, v)
	case *ExprStatement:
		walk(n.Expression, v)
	case *TupleType:
		for _, field := range n.Elts.List {
			walk(field, v)
		}
	case *Field:
		for _, name := range n.Names {
			walk(name, v)
		}
		walk(n.Typ, v)
	case *CallExpr:
		walk(n.Fun, v)
		for _, arg := range n.Args {
			walk(arg, v)
		}
	case *DotExpr:
		walk(n.X, v)
		walk(n.Attr, v)
	case *BinaryExpr:
		walk(n.Left, v)
		walk(n.Right, v)
	case *UnaryExpr:
		walk(n.Right, v)
	case *MatchAssignExpr:
		walk(n.Left, v)
		walk(n.Right, v)
	case *AssignExpr:
		walk(n.Left, v)
		walk(n.Right, v)
	case *ListLiteral:
		for _, elt := range n.Elts {
			walk(elt, v)
		}
	case *ListType:
		walk(n.Elt, v)
	default:
		// leaf node, no need to do anything
		return
	}
}

// Walk traverses each node calling v.Walk for each one, including the root first. If v.Walk returns an error,
// then the traversal stops and the error is returned to the caller.
func Walk(n Node, v Visitor) (result error) {
	defer func() {
		err := recover()
		if berr, ok := err.(bailout); ok {
			result = berr
		} else if err != nil {
			panic(err)
		}
	}()
	walk(n, v)
	return
}
