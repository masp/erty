package token

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestTokenTypeSTring(t *testing.T) {
	for i := Invalid; i < EOF; i += 1 {
		require.NotPanics(t, func() { _ = i.String() })
	}
}
