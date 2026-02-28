# General instructions
- Don't comment code unless a comment is truly warranted

# Go projects
- Run go build to ensure the code can build
- Run go test if there are tests in the project
- For documentation about go libraries, use go doc
- Do not manually update go.mod or go.sum. Always use `go get` and `go mod` to update those files. Do not guess what tag version to use, prefer latest by default
- Prefer separate lines for a function call that returns an error and the if err != nil check
- Do not use ioutil, it is deprecated
