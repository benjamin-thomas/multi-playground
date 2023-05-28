package parser

import (
	"reflect"
	"strings"
	"testing"
)

func TestValid(t *testing.T) {
	testValidStmt(t, "SELECT * FROM books", &SelectStatement{Fields: []string{"*"}, TableName: "books"})
	testValidStmt(t, "SELECT   id  ,title    FROM books", &SelectStatement{Fields: []string{"id", "title"}, TableName: "books"})
}

func TestInvalid(t *testing.T) {
	testInValidStmt(t, "FROM books", "found \"FROM\", expected SELECT")
	testInValidStmt(t, "SELECT id,,title FROM books", "found \",\", expected field")
	testInValidStmt(t, "SELECT id,title FROM", "found \"\", expected table name")
}

func testValidStmt(t *testing.T, sql string, exp *SelectStatement) {
	got, err := newParser(strings.NewReader(sql)).parse()
	if err != nil {
		t.Errorf("Got failure:%s\n", err)
	}
	if !reflect.DeepEqual(exp, got) {
		t.Errorf("Expected: %#v, GOT %#v", exp, got)
	}
}

func testInValidStmt(t *testing.T, sql string, expErr string) {
	got, err := newParser(strings.NewReader(sql)).parse()
	if err.Error() != expErr {
		t.Errorf("Expected error: '%s', got '%s' (%#v)", expErr, err, got)
	}
}
