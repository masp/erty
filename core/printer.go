package core

import (
	"fmt"
	"io"
	"strings"
)

func NewPrinter(w io.Writer) *Printer {
	return &Printer{
		Output:     w,
		indentSize: 4,
	}
}

// Printer converts the core AST into a string representation which has the .core extension. The .core file is generated
// equivalent by the erlc command `erlc +to_core <file>.erl`.
type Printer struct {
	Output                 io.Writer
	indentSize, currIndent int
}

func (c *Printer) indent() {
	c.currIndent++
}

func (c *Printer) dedent() {
	c.currIndent--
}

func (c *Printer) emitln() {
	c.Output.Write([]byte{'\n'})
	io.WriteString(c.Output, strings.Repeat(" ", c.indentSize*c.currIndent))
}

func (c *Printer) emitf(format string, args ...interface{}) {
	fmt.Fprintf(c.Output, format, args...)
}

func (c *Printer) PrintModule(mod *Module) {
	c.emitf("module '%s' [", mod.Name)
	for i, fn := range mod.Exports {
		if i > 0 {
			c.emitf(",")
		}
		c.emitf("%s", fn.String())
	}
	c.emitf("]")

	c.indent()
	c.emitln()
	c.emitAttrs(mod.Attributes)
	c.dedent()

	c.emitln()
	for _, def := range mod.Functions {
		c.PrintFunc(def)
	}
	c.emitf("end")
}

func (c *Printer) emitAttrs(attrs []*Attribute) {
	c.emitf("attributes [")
	c.indent()
	c.emitln()
	for i, attr := range attrs {
		if i > 0 {
			c.emitf(",")
			c.emitln()
		}
		c.emitAttr(attr)
	}
	c.dedent()
	c.emitf("]")
}

func (c *Printer) emitAttr(attr *Attribute) {
	c.emitf("'%s' =", attr.Key.Value)
	c.indent()
	c.emitln()
	c.emitConst(attr.Value)
	c.dedent()
}

func (c *Printer) emitConst(cnst Const) {
	switch cnst := cnst.(type) {
	case Literal:
		c.emitLiteral(cnst)
	case *ConstTuple:
		c.emitf("{")
		for i, elem := range cnst.Elements {
			if i > 0 {
				c.emitf(",")
			}
			c.emitConst(elem)
		}
		c.emitf("}")
	case *ConstList:
		cons := cnst.Elements
		for {
			c.emitf("[")
			if len(cons) == 0 {
				c.emitf(strings.Repeat("]", len(cnst.Elements)+1))
				break
			}
			c.emitConst(cons[0])
			c.emitf("|")
			cons = cons[1:]
		}
	default:
		panic(fmt.Sprintf("unhandled const type %T", cnst))
	}
}

func (c *Printer) PrintFunc(fn *Func) {
	c.emitFnHeader(fn)
	c.emitFn(fn)
	c.dedent()
	c.emitln()
}

func (c *Printer) emitFnHeader(fn *Func) {
	c.emitf("%s =", fn.Name.String())
	c.indent()
	c.emitln()
}

func (c *Printer) emitFn(fn *Func) {
	c.beginAnnotation(fn)
	c.emitf("fun (")
	for i, param := range fn.Parameters {
		if i > 0 {
			c.emitf(",")
		}
		c.emitf("%s", param.Name)
	}
	c.emitf(") ->")
	c.indent()
	c.emitln()
	c.emitExpr(fn.Body)

	c.endAnnotation(fn)
	c.dedent()
}

func (c *Printer) beginAnnotation(ann Annotated) {
	if len(ann.Annotations().Attrs) > 0 {
		c.emitf("(")
	}
}

func (c *Printer) endAnnotation(ann Annotated) {
	if len(ann.Annotations().Attrs) > 0 {
		c.emitln()
		c.emitf("-| [")
		if ann != nil {
			for i, attr := range ann.Annotations().Attrs {
				if i > 0 {
					c.emitf(",")
				}
				c.emitConst(attr)
			}
		}
		c.emitf("])")
	}
}

func (c *Printer) emitExpr(expr Expr) {
	switch expr := expr.(type) {
	case Literal:
		c.emitLiteral(expr)
	case *FuncName:
		c.emitf("%s", expr.String())
	case *Var:
		c.beginAnnotation(expr)
		c.emitf("%s", expr.Name)
		c.endAnnotation(expr)
	case *Func:
		c.emitFn(expr)
	case *InterModuleCall:
		c.emitInterModuleCall(expr)
	case *ApplyExpr:
		c.emitApply(expr)
	case *LetExpr:
		c.emitLet(expr)
	case *DoExpr:
		c.emitDo(expr)
	default:
		panic(fmt.Sprintf("unknown expression type %T", expr))
	}
}

func (c *Printer) emitLiteral(lit Literal) {
	switch lit := lit.(type) {
	case Integer:
		c.emitf("%d", lit.Value)
	case Float:
		c.emitf("%f", lit.Value)
	case Atom:
		c.emitf("'%s'", lit.Value)
	case String:
		c.emitf("\"%s\"", lit.Value)
	default:
		panic(fmt.Sprintf("unknown literal type %T", lit))
	}
}

func (c *Printer) emitInterModuleCall(call *InterModuleCall) {
	c.emitf("call ")
	c.emitExpr(call.Module)
	c.emitf(":")
	c.emitExpr(call.Func)
	c.indent()
	c.emitln()
	c.emitf("(")
	for i, arg := range call.Args {
		if i > 0 {
			c.emitf(",")
		}
		c.emitExpr(arg)
	}
	c.emitf(")")
	c.dedent()
}

func (c *Printer) emitApply(app *ApplyExpr) {
	c.emitf("apply ")
	c.emitExpr(app.Func)
	c.indent()
	c.emitln()
	c.emitf("(")
	for i, arg := range app.Args {
		if i > 0 {
			c.emitf(",")
		}
		c.emitExpr(arg)
	}
	c.emitf(")")
	c.dedent()
}

func (c *Printer) emitLet(let *LetExpr) {
	c.emitf("let ")
	c.emitf("<")
	for i, binding := range let.Vars {
		if i > 0 {
			c.emitf(",")
		}
		c.emitf(binding.Name)
	}
	c.emitf("> =")
	c.indent()
	c.emitln()
	c.emitExpr(let.Assign)
	c.dedent()
	c.emitln()
	c.emitf("in ")
	c.emitExpr(let.In)
}

func (c *Printer) emitDo(do *DoExpr) {
	c.emitf("do ")
	c.indent()
	defer c.dedent()
	c.emitln()
	c.emitExpr(do.Before)
	c.emitln()
	c.emitExpr(do.After)
}
