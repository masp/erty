// Package core provides Go structs representing Erlang Core AST.
package core

import (
	"fmt"

	"github.com/masp/garlang/ast"
)

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
	Exports    []*FuncName
	Attributes []*Attribute
	Functions  []*Func
}

type FuncName struct {
	Name  string
	Arity int
}

func (*FuncName) isConst() {}
func (*FuncName) isExpr()  {}
func (f *FuncName) String() string {
	return fmt.Sprintf("'%s'/%d", f.Name, f.Arity)
}

type Attribute struct {
	Key   Atom
	Value Const
}

type Expr interface {
	isExpr()
}

// let <v0, v1, ..., vn> = e_assign in e_in
type LetExpr struct {
	Vars   []*Var
	Assign Expr
	In     Expr
}

func (*LetExpr) isExpr() {}

// do e1, e2 sequences e1 then e2, evaluating to e2
type DoExpr struct {
	Before, After Expr
}

func (*DoExpr) isExpr() {}

// Arity has form 'name'/n
type Arity struct {
	Name  Atom
	Arity int
}

func (*Arity) isExpr() {}

// apply exprs0/arity (exprs1, . . ., exprsn)
type ApplyExpr struct {
	Func Expr
	Args []Expr
}

func (*ApplyExpr) isExpr() {}

type InterModuleCall struct {
	Module Expr
	Func   Expr
	Args   []Expr
}

func (*InterModuleCall) isExpr() {}

type Func struct {
	annotated
	Name       *FuncName
	Parameters []*Var
	Body       Expr
}

type Annotated interface {
	Annotate(attr Const)
	Annotations() *Annotation
}

type annotated struct {
	Annotation
}

func (h *annotated) Annotate(attr Const)      { h.Annotation.Attrs = append(h.Annotation.Attrs, attr) }
func (h *annotated) Annotations() *Annotation { return &h.Annotation }

type Annotation struct {
	Attrs []Const
}

func (*Func) isExpr() {}

type Var struct {
	annotated
	OriginalName string // can be any string

	Type ast.Type
	Name string // must be valid Erlang name (start with uppercase or _)
}

func (*Var) isExpr() {}

type BadExpr struct{}

func (*BadExpr) isExpr() {}

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

type List struct {
	Elements []Expr
}

func (*List) isExpr() {}

// Const is used only in the attributes, whereas the ExprList/Tuple are used in the body of the function.
type Const interface {
	isConst()
}

type ConstList struct {
	Elements []Const
}

func (*ConstList) isConst() {}

type ConstTuple struct {
	Elements []Const
}

func (*ConstTuple) isConst() {}

type Binary struct {
	Bits []*Bitstring
}

func (*Binary) isExpr()    {}
func (*Binary) isLiteral() {}

var (
	// These combination of flags are used very often, so store a single version
	BitStringFlags = &ConstList{Elements: []Const{BitFlagUnsigned, BitFlagBig}}

	BitFlagBig      = Atom{Value: "big"}
	BitFlagLittle   = Atom{Value: "little"}
	BitFlagSigned   = Atom{Value: "signed"}
	BitFlagUnsigned = Atom{Value: "unsigned"}

	BitTypeInteger = Atom{Value: "integer"}
	BitTypeBinary  = Atom{Value: "binary"}

	BitLengthAll = Atom{Value: "all"}

	BitUnitBit  = 1
	BitUnitByte = 8
)

type Bitstring struct {
	Var *Var // nil if Bitstring is a constant
	Val Expr // nil if Bitstring is a variable

	Length Const      // number of bits/number of bytes/'all' if /binary style is used (see BitLength*)
	Unit   int        // 1 = bit, 8 = byte (see BitSize*)
	Type   Atom       // e.g. integer, binary
	Flags  *ConstList // list of flags (usually atoms) like 'unsigned', 'big', 'little', 'signed
}
