// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Source is modified from Go's excellent internal token package.
// Ref: https://cs.opensource.google/go/go/+/refs/tags/go1.17.3:src/go/token/position.go

package token

import (
	"fmt"
	"sort"
	"sync"
)

// -----------------------------------------------------------------------------
// Positions

// Position describes an arbitrary source position
// including the file, line, and column location.
// A Position is valid if the line number is > 0.
type Position struct {
	Filename string // filename, if any
	Offset   Pos    // offset, starting at 1
	Line     int    // line number, starting at 1
	Column   int    // column number, starting at 1 (byte count)
}

// IsValid reports whether the position is valid.
func (pos *Position) IsValid() bool { return pos.Line > 0 }

// String returns a string in one of several forms:
//
//	file:line:column    valid position with file name
//	file:line           valid position with file name but no column (column == 0)
//	line:column         valid position without file name
//	line                valid position without file name and no column (column == 0)
//	file                invalid position with file name
//	-                   invalid position without file name
func (pos Position) String() string {
	s := pos.Filename
	if pos.IsValid() {
		if s != "" {
			s += ":"
		}
		s += fmt.Sprintf("%d", pos.Line)
		if pos.Column != 0 {
			s += fmt.Sprintf(":%d", pos.Column)
		}
	}
	if s == "" {
		s = "-"
	}
	return s
}

// Pos is a compact encoding of a source position within a file set.
// It can be converted into a Position for a more convenient, but much
// larger, representation.
//
// Pos = offset in the File
type Pos int

// The zero value for Pos is NoPos; there is no file and line information
// associated with it, and NoPos.IsValid() is false. NoPos is always
// smaller than any other Pos value. The corresponding Position value
// for NoPos is the zero value for Position.
const NoPos Pos = 0

// IsValid reports whether the position is valid.
func (p Pos) IsValid() bool {
	return p != NoPos
}

// Offset returns file offset
func (p Pos) Offset() int {
	return int(p) - 1
}

// -----------------------------------------------------------------------------
// File

// A File is a handle for an opened file.
// A File has a name, size, and line offset table.
type File struct {
	Name string // file name as provided
	Size int    // file size in bytes

	lineMut *sync.Mutex // guards lines for threadsafe access
	lines   []int       // lines contains the offset of the first character for each line (the first entry is always 0)
}

func NewFile(name string, size int) *File {
	f := &File{
		Name:    name,
		Size:    size,
		lineMut: &sync.Mutex{},
		lines:   []int{-1}, // implicit newline at start of file
	}
	return f
}

// LineCount returns the number of lines in file f.
func (f *File) LineCount() int {
	f.lineMut.Lock()
	n := len(f.lines)
	f.lineMut.Unlock()
	return n
}

// AddLine adds the line offset for a new line.
// The line offset must be larger than the offset for the previous line
// and smaller than the file size; otherwise the line offset is ignored.
func (f *File) AddLine(offset int) {
	f.lineMut.Lock()
	if i := len(f.lines); (i == 0 || f.lines[i-1] < offset+1) && offset < f.Size {
		f.lines = append(f.lines, offset)
	}
	f.lineMut.Unlock()
}

// Pos returns the Pos value for the given file offset;
// the offset must be <= f.Size().
// f.Pos(f.Offset(p)) == p.
func (f *File) Pos(offset int) Pos {
	if offset > f.Size {
		panic(fmt.Sprintf("invalid file offset %d (should be <= %d)", offset, f.Size))
	}
	return Pos(offset + 1)
}

// LineStart returns the offset in the file for the start of line X, line starting at 1.
// If out of bounds, the line returned is closest to the requested line.
func (f *File) LineStart(line int) int {
	if line <= 0 {
		panic("line must be >0")
	}
	if line > f.LineCount() {
		return f.Size - 1
	}
	f.lineMut.Lock()
	defer f.lineMut.Unlock()
	// f.lines is the offset of the newline character before the line (with -1 at the start)
	return f.lines[line-1] + 1
}

// Line returns the line number for the given file position p;
// p must be a Pos value in that file or NoPos.
func (f *File) Line(p Pos) int {
	return f.Position(p).Line
}

// PositionFor returns the Position value for the given file position p.
// If adjusted is set, the position may be adjusted by position-altering
// //line comments; otherwise those comments are ignored.
// p must be a Pos value in f or NoPos.
func (f *File) Position(p Pos) (pos Position) {
	pos = Position{Filename: f.Name, Offset: p}
	if p != NoPos {
		offset := int(p) - 1
		if offset < 0 || offset >= f.Size {
			panic(fmt.Sprintf("invalid Pos value %d (should be in [%d, %d])", p, 1, f.Size))
		}
		f.lineMut.Lock()
		defer f.lineMut.Unlock()
		pos.Line = sort.SearchInts(f.lines, offset)
		pos.Column = offset - f.lines[pos.Line-1]
	}
	return
}
