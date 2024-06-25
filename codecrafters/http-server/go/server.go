package main

import (
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"path"
	"strconv"
	"strings"
)

/*
rg --files | entr -cr go run ./server.go
*/

func must[T any](value T, err error) T {
	if err != nil {
		panic(err)
	}
	return value
}

func must1(err error) {
	if err != nil {
		panic(err)
	}
}

func findArgOr(args []string, key string, default_ string) string {
	for i := range args {
		arg := args[i]
		if arg == key && len(args) > i+1 {
			return args[i+1]
		}
	}
	return default_
}

func main() {
	log.Println("Booting up...")
	args := os.Args[1:]
	filesDir := findArgOr(args, "--directory", "/dev/null")
	log.Println("filesDir:", filesDir)

	runServer(filesDir)
}

type Request struct {
	Verb    string
	Path    string
	PathArg string
	Headers map[string]string
	Body    string
}

func toRequest(raw []byte) (req Request, err error) {
	all := strings.SplitN(string(raw), "\r\n\r\n", 2)
	if len(all) < 2 {
		return Request{}, errors.New("malformed request (1)")
	}

	preBody := all[0]
	body := all[1]
	preBodyLines := strings.Split(preBody, "\r\n")
	if len(preBodyLines) < 1 {
		return Request{}, errors.New("malformed request (2)")
	}

	elems := strings.Split(preBodyLines[0], " ")
	if len(elems) < 2 {
		return Request{}, errors.New("malformed request (3)")
	}
	verb := elems[0]
	paths := strings.SplitN(elems[1], "/", 3)
	path_ := paths[1]
	var pathArg string
	if len(paths) < 3 {
		pathArg = ""
	} else {
		pathArg = paths[2]
	}

	headersRaw := preBodyLines[1:]
	headers := make(map[string]string)
	for _, line := range headersRaw {
		elems2 := strings.SplitN(line, ":", 2)
		if len(elems2) < 2 {
			return Request{}, errors.New("malformed header")
		}
		key := strings.TrimSpace(elems2[0])
		val := strings.TrimSpace(elems2[1])
		headers[key] = val
	}

	req = Request{verb, path_, pathArg, headers, body}
	return req, nil
}

func handleRequest(filesDir string, readRaw func() []byte,
	closeConn func(), ok func() int, badRequest func() int, notFound func() int, serverError func() int, writeln func(s string) int, crlf func() int) {
	defer closeConn()
	log.Println("Accepted connection")
	log.Println("Read request")

	requestRaw := readRaw()
	req, err := toRequest(requestRaw)
	if err != nil {
		badRequest()
		return
	}
	log.Printf("Got request: %+v", req)

	switch req.Verb {

	case "GET":
		switch req.Path {
		case "":
			ok()
			crlf()
		case "echo":
			ok()
			writeln("Content-Type: text/plain")
			writeln(fmt.Sprintf("Content-Length: %d", len(req.PathArg)))
			crlf()
			writeln(req.PathArg)
		case "user-agent":
			userAgent := req.Headers["User-Agent"]
			ok()
			writeln("Content-Type: text/plain")
			writeln(fmt.Sprintf("Content-Length: %d", len(userAgent)))
			crlf()
			writeln(userAgent)
		case "files":
			filepath := path.Join(filesDir, req.PathArg)
			content, err := os.ReadFile(filepath)
			if err != nil {
				if errors.Is(err, os.ErrNotExist) {
					notFound()
				} else {
					serverError()
				}
			} else {
				ok()
				writeln("Content-Type: application/octet-stream")
				writeln(fmt.Sprintf("Content-Length: %d", len(content)))
				crlf()
				writeln(string(content))
			}
		case "hello":
			ok()
			crlf()
			writeln("How are you?")
		default:
			notFound()
		}
	case "POST":
		switch req.Path {
		case "files":
			bodyLen, err := strconv.Atoi(req.Headers["Content-Length"]) // I get garbage from the buffer otherwise
			if err != nil {
				bodyLen = 0
			}
			content := req.Body[0:bodyLen]
			log.Printf("content=%s\n", content)
			filepath := path.Join(filesDir, req.PathArg)
			err = os.WriteFile(filepath, []byte(content), 0644)
			if err != nil {
				serverError()
			} else {
				writeln("HTTP/1.1 201 Created")
				writeln("Content-Type: application/octet-stream")
				writeln(fmt.Sprintf("Content-Length: %d", len(content)))
				crlf()
				writeln(content)
			}

		}
	default:
		notFound()
	}
	log.Println("Wrote response")
}

func handleConn(listen net.Listener, filesDir string) {
	for {
		fmt.Println("---")
		conn := must(listen.Accept())

		// HELPERS START
		writeln := func(s string) int {
			return must(conn.Write([]byte(s + "\r\n")))
		}
		crlf := func() int {
			// This function is mostly used to signal the end of the headers
			// While HTTP clients such as curl don't really need this line set, the test cases do require them
			return must(conn.Write([]byte("\r\n")))
		}

		ok := func() int {
			return writeln("HTTP/1.1 200 OK")
		}

		badRequest := func() int {
			return writeln("HTTP/1.1 400 Bad Request") + crlf()
		}

		serverError := func() int {
			return writeln("HTTP/1.1 500 Internal Server Error") + crlf()
		}

		notFound := func() int {
			n := writeln("HTTP/1.1 404 Not Found")
			m := crlf()
			return n + m
		}
		readRaw := func() []byte {
			requestRaw := make([]byte, 1024)
			_ = must(conn.Read(requestRaw))
			return requestRaw
		}

		closeConn := func() {
			must1(conn.Close())
		}
		// HELPERS END

		go handleRequest(filesDir, readRaw,
			closeConn, ok, badRequest, notFound, serverError, writeln, crlf)
	}
}

// It's unclear how or if any error handling should be done here, so I choose to just panic for now.
func runServer(filesDir string) {
	listen := must(net.Listen("tcp", "127.0.0.1:4221"))
	handleConn(listen, filesDir)
	must1(listen.Close())
}
