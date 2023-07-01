package types

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestApplyOp(t *testing.T) {
	tests := map[string]struct {
		t1, t2  Type
		want    Type
		wantErr error
	}{
		"matching integer types":              {Int, Int, Int, nil},
		"untyped int with integer":            {UntypedInt, Int, Int, nil},
		"integer with float":                  {Int, Float, nil, ErrMismatch},
		"untyped float with float":            {UntypedFloat, Float, Float, nil},
		"untyped string with mismatched type": {UntypedString, Float, nil, ErrMismatch},
		"untyped string with matching string": {UntypedString, String, String, nil},
		"untyped and untyped":                 {UntypedInt, UntypedInt, UntypedInt, nil},
		"untyped int with untyped float":      {UntypedInt, UntypedFloat, UntypedFloat, nil},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got, err := ApplyOp(tt.t1, tt.t2)
			assert.Equal(t, tt.want, got)
			assert.Equal(t, tt.wantErr, err)
		})
	}
}
