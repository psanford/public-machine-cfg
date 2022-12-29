//go:build tools
// +build tools

package gotools

import (
	_ "github.com/ChimeraCoder/gojson/gojson"
	_ "github.com/FiloSottile/captive-browser"
	_ "github.com/fatih/gomodifytags"
	_ "github.com/go-delve/delve/cmd/dlv"
	_ "github.com/josharian/impl"
	_ "github.com/psanford/gist"
	_ "github.com/psanford/ppjson"
	_ "github.com/skanehira/gjo"
	_ "golang.org/x/mobile/cmd/gobind"
	_ "golang.org/x/mobile/cmd/gomobile"
	_ "golang.org/x/tools/cmd/godoc"
	_ "golang.org/x/tools/cmd/goimports"
	_ "golang.org/x/tools/gopls"
)
