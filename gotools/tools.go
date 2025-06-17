//go:build tools
// +build tools

package gotools

import (
	_ "github.com/ChimeraCoder/gojson/gojson"
	_ "github.com/FiloSottile/captive-browser"
	_ "github.com/fatih/gomodifytags"
	_ "github.com/go-delve/delve/cmd/dlv"
	_ "github.com/gokrazy/tools/cmd/gok"
	_ "github.com/josharian/impl"
	_ "github.com/psanford/ppjson"
	_ "github.com/skanehira/gjo"
	_ "golang.org/x/tools/cmd/goimports"
	_ "golang.org/x/tools/gopls"
	_ "golang.org/x/vuln/cmd/govulncheck"
)
