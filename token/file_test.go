package token

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPositions(t *testing.T) {
	src := []byte("main here\nline1\nline2\nline3")

	file := NewFile("<test>", len(src))
	for offset, c := range src {
		if c == '\n' {
			file.AddLine(offset)
		}
	}

	tests := []struct {
		offset int
		pos    Pos
		line   int
		col    int
	}{
		{0, 1, 1, 1},
		{5, 6, 1, 6},
		{10, 11, 2, 1},
		{26, 27, 4, 5},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("offset=%d", tt.offset), func(t *testing.T) {
			pos := file.Pos(tt.offset)
			position := file.Position(pos)
			assert.Equal(t, tt.pos, pos, "expected position to match")
			assert.Equal(t, tt.line, position.Line, "expected line to match")
			assert.Equal(t, tt.col, position.Column, "expected column to match")
		})
	}
}

func TestLineStart(t *testing.T) {
	src := "a\nb\nc\n"

	file := NewFile("<test>", len(src))
	for offset, c := range src {
		if c == '\n' {
			file.AddLine(offset)
		}
	}

	tests := []struct {
		line   int
		offset int
	}{
		{1, 0},
		{2, 2},
		{3, 4},
		{4, 6},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("line=%d", tt.line), func(t *testing.T) {
			offset := file.LineStart(tt.line)
			assert.Equal(t, tt.offset, offset, "expected offset to match")
		})
	}
}
