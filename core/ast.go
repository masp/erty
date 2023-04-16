// Package core provides Go structs representing Erlang Core AST.
package core

import "fmt"

// The definition of the Erlang core is defined at https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf
//
// module ::= module Atom [ fnamei1
// , . . ., fnameik
// ]
// attributes [ Atom1 = const1, . . ., Atomm = constm ]
// fname1 = fun1 · · · fnamen = funn end
//
// fname ::= Atom / Integer
// const ::= lit | [ const1 | const2 ] | { const1, . . ., constn }
// lit ::= Integer | Float | Atom
// | Char | String | [ ]
// fun ::= fun (var 1, . . ., var n) -> exprs
// var ::= VariableName
// exprs ::= expr | < expr 1, . . ., expr n >
// expr ::= var | fname | lit | fun
// | [ exprs1 | exprs2 ] | { exprs1, . . ., exprsn }
// | let vars = exprs1 in exprs2
// | case exprs of clause1 · · · clausen end
// | letrec fname1 = fun1 · · · fnamen = funn in exprs
// | apply exprs0(exprs1, . . ., exprsn)
// | call expr:expr(exprs1, . . ., exprsn)
// | primop Atom(exprs1, . . ., exprsn)
// | receive clause1 · · · clausen after exprs1 -> exprs2
// | try exprs1 of <var 1, . . .var n> -> exprs2
//
//	catch <var n+1, . . .var n+m> -> exprs3
//
// | do exprs1 exprs2
// | catch exprs
// vars ::= var | < var 1, . . ., var n >
// clause ::= pats when exprs1 -> exprs2
// pats ::= pat | < pat1, . . ., patn >
// pat ::= var | lit | [ pat1 | pat2 ] | { pat1, . . ., patn }
// | var = pat
type Module struct {
	Name       string
	Exports    []FuncName
	Attributes []Attribute
	Functions  []Func
}

type FuncName struct {
	Name  string
	Arity int
}

func (FuncName) isConst() {}
func (FuncName) isExpr()  {}
func (f FuncName) String() string {
	return fmt.Sprintf("'%s'/%d", f.Name, f.Arity)
}

type Attribute struct {
	Key   Atom
	Value Const
}

type Expr interface {
	isExpr()
}

// apply exprs0(exprs1, . . ., exprsn)
type Application struct {
	Func Expr
	Args []Expr
}

func (Application) isExpr() {}

type InterModuleCall struct {
	Module Expr
	Func   Expr
	Args   []Expr
}

func (InterModuleCall) isExpr() {}

type Func struct {
	Name       FuncName
	Parameters []Var
	Body       Expr
	Annotation Annotation
}

type Annotation struct {
	Attrs []Const
}

func (Func) isExpr() {}

type Var struct {
	Name string
}

func (Var) isExpr() {}

type Literal interface {
	Const
	isLiteral()
}

type Atom struct {
	Value string
}

func (Atom) isLiteral() {}
func (Atom) isConst()   {}
func (Atom) isExpr()    {}

type Integer struct {
	Value int64
}

func (Integer) isLiteral() {}
func (Integer) isConst()   {}
func (Integer) isExpr()    {}

type Float struct {
	Value float64
}

func (Float) isLiteral() {}
func (Float) isConst()   {}
func (Float) isExpr()    {}

type Char struct {
	Value rune
}

func (Char) isLiteral() {}
func (Char) isConst()   {}
func (Char) isExpr()    {}

type String struct {
	Value string
}

func (String) isLiteral() {}
func (String) isConst()   {}
func (String) isExpr()    {}

// Const is used only in the attributes, whereas the ExprList/Tuple are used in the body of the function.
type Const interface {
	isConst()
}

type ConstList struct {
	Elements []Const
}

func (ConstList) isConst() {}

type ConstTuple struct {
	Elements []Const
}

func (ConstTuple) isConst() {}
