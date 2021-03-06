// Code generated by SQLBoiler 3.7.1 (https://github.com/volatiletech/sqlboiler). DO NOT EDIT.
// This file is meant to be re-generated in place and/or deleted at any time.

package models

import "testing"

// This test suite runs each operation test in parallel.
// Example, if your database has 3 tables, the suite will run:
// table1, table2 and table3 Delete in parallel
// table1, table2 and table3 Insert in parallel, and so forth.
// It does NOT run each operation group in parallel.
// Separating the tests thusly grants avoidance of Postgres deadlocks.
func TestParent(t *testing.T) {
	t.Run("Books", testBooks)
	t.Run("BooksLogs", testBooksLogs)
	t.Run("People", testPeople)
	t.Run("PeopleLogs", testPeopleLogs)
}

func TestDelete(t *testing.T) {
	t.Run("Books", testBooksDelete)
	t.Run("BooksLogs", testBooksLogsDelete)
	t.Run("People", testPeopleDelete)
	t.Run("PeopleLogs", testPeopleLogsDelete)
}

func TestQueryDeleteAll(t *testing.T) {
	t.Run("Books", testBooksQueryDeleteAll)
	t.Run("BooksLogs", testBooksLogsQueryDeleteAll)
	t.Run("People", testPeopleQueryDeleteAll)
	t.Run("PeopleLogs", testPeopleLogsQueryDeleteAll)
}

func TestSliceDeleteAll(t *testing.T) {
	t.Run("Books", testBooksSliceDeleteAll)
	t.Run("BooksLogs", testBooksLogsSliceDeleteAll)
	t.Run("People", testPeopleSliceDeleteAll)
	t.Run("PeopleLogs", testPeopleLogsSliceDeleteAll)
}

func TestExists(t *testing.T) {
	t.Run("Books", testBooksExists)
	t.Run("BooksLogs", testBooksLogsExists)
	t.Run("People", testPeopleExists)
	t.Run("PeopleLogs", testPeopleLogsExists)
}

func TestFind(t *testing.T) {
	t.Run("Books", testBooksFind)
	t.Run("BooksLogs", testBooksLogsFind)
	t.Run("People", testPeopleFind)
	t.Run("PeopleLogs", testPeopleLogsFind)
}

func TestBind(t *testing.T) {
	t.Run("Books", testBooksBind)
	t.Run("BooksLogs", testBooksLogsBind)
	t.Run("People", testPeopleBind)
	t.Run("PeopleLogs", testPeopleLogsBind)
}

func TestOne(t *testing.T) {
	t.Run("Books", testBooksOne)
	t.Run("BooksLogs", testBooksLogsOne)
	t.Run("People", testPeopleOne)
	t.Run("PeopleLogs", testPeopleLogsOne)
}

func TestAll(t *testing.T) {
	t.Run("Books", testBooksAll)
	t.Run("BooksLogs", testBooksLogsAll)
	t.Run("People", testPeopleAll)
	t.Run("PeopleLogs", testPeopleLogsAll)
}

func TestCount(t *testing.T) {
	t.Run("Books", testBooksCount)
	t.Run("BooksLogs", testBooksLogsCount)
	t.Run("People", testPeopleCount)
	t.Run("PeopleLogs", testPeopleLogsCount)
}

func TestHooks(t *testing.T) {
	t.Run("Books", testBooksHooks)
	t.Run("BooksLogs", testBooksLogsHooks)
	t.Run("People", testPeopleHooks)
	t.Run("PeopleLogs", testPeopleLogsHooks)
}

func TestInsert(t *testing.T) {
	t.Run("Books", testBooksInsert)
	t.Run("Books", testBooksInsertWhitelist)
	t.Run("BooksLogs", testBooksLogsInsert)
	t.Run("BooksLogs", testBooksLogsInsertWhitelist)
	t.Run("People", testPeopleInsert)
	t.Run("People", testPeopleInsertWhitelist)
	t.Run("PeopleLogs", testPeopleLogsInsert)
	t.Run("PeopleLogs", testPeopleLogsInsertWhitelist)
}

// TestToOne tests cannot be run in parallel
// or deadlocks can occur.
func TestToOne(t *testing.T) {
	t.Run("BookToPersonUsingAuthorId", testBookToOnePersonUsingAuthorId)
	t.Run("BooksLogToPersonUsingAuthorNew", testBooksLogToOnePersonUsingAuthorNew)
	t.Run("BooksLogToPersonUsingAuthorOld", testBooksLogToOnePersonUsingAuthorOld)
	t.Run("BooksLogToBookUsingBookIdNew", testBooksLogToOneBookUsingBookIdNew)
	t.Run("BooksLogToBookUsingBookIdOld", testBooksLogToOneBookUsingBookIdOld)
	t.Run("PeopleLogToPersonUsingPeopleIdNew", testPeopleLogToOnePersonUsingPeopleIdNew)
	t.Run("PeopleLogToPersonUsingPeopleIdOld", testPeopleLogToOnePersonUsingPeopleIdOld)
}

// TestOneToOne tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOne(t *testing.T) {}

// TestToMany tests cannot be run in parallel
// or deadlocks can occur.
func TestToMany(t *testing.T) {
	t.Run("BookToBookIdNewBooksLogs", testBookToManyBookIdNewBooksLogs)
	t.Run("BookToBookIdOldBooksLogs", testBookToManyBookIdOldBooksLogs)
	t.Run("PersonToAuthorIdBooks", testPersonToManyAuthorIdBooks)
	t.Run("PersonToAuthorNewBooksLogs", testPersonToManyAuthorNewBooksLogs)
	t.Run("PersonToAuthorOldBooksLogs", testPersonToManyAuthorOldBooksLogs)
	t.Run("PersonToPeopleIdNewPeopleLogs", testPersonToManyPeopleIdNewPeopleLogs)
	t.Run("PersonToPeopleIdOldPeopleLogs", testPersonToManyPeopleIdOldPeopleLogs)
}

// TestToOneSet tests cannot be run in parallel
// or deadlocks can occur.
func TestToOneSet(t *testing.T) {
	t.Run("BookToPersonUsingAuthorIdBooks", testBookToOneSetOpPersonUsingAuthorId)
	t.Run("BooksLogToPersonUsingAuthorNewBooksLogs", testBooksLogToOneSetOpPersonUsingAuthorNew)
	t.Run("BooksLogToPersonUsingAuthorOldBooksLogs", testBooksLogToOneSetOpPersonUsingAuthorOld)
	t.Run("BooksLogToBookUsingBookIdNewBooksLogs", testBooksLogToOneSetOpBookUsingBookIdNew)
	t.Run("BooksLogToBookUsingBookIdOldBooksLogs", testBooksLogToOneSetOpBookUsingBookIdOld)
	t.Run("PeopleLogToPersonUsingPeopleIdNewPeopleLogs", testPeopleLogToOneSetOpPersonUsingPeopleIdNew)
	t.Run("PeopleLogToPersonUsingPeopleIdOldPeopleLogs", testPeopleLogToOneSetOpPersonUsingPeopleIdOld)
}

// TestToOneRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestToOneRemove(t *testing.T) {
	t.Run("BookToPersonUsingAuthorIdBooks", testBookToOneRemoveOpPersonUsingAuthorId)
	t.Run("BooksLogToPersonUsingAuthorNewBooksLogs", testBooksLogToOneRemoveOpPersonUsingAuthorNew)
	t.Run("BooksLogToPersonUsingAuthorOldBooksLogs", testBooksLogToOneRemoveOpPersonUsingAuthorOld)
	t.Run("BooksLogToBookUsingBookIdNewBooksLogs", testBooksLogToOneRemoveOpBookUsingBookIdNew)
	t.Run("BooksLogToBookUsingBookIdOldBooksLogs", testBooksLogToOneRemoveOpBookUsingBookIdOld)
	t.Run("PeopleLogToPersonUsingPeopleIdNewPeopleLogs", testPeopleLogToOneRemoveOpPersonUsingPeopleIdNew)
	t.Run("PeopleLogToPersonUsingPeopleIdOldPeopleLogs", testPeopleLogToOneRemoveOpPersonUsingPeopleIdOld)
}

// TestOneToOneSet tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOneSet(t *testing.T) {}

// TestOneToOneRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOneRemove(t *testing.T) {}

// TestToManyAdd tests cannot be run in parallel
// or deadlocks can occur.
func TestToManyAdd(t *testing.T) {
	t.Run("BookToBookIdNewBooksLogs", testBookToManyAddOpBookIdNewBooksLogs)
	t.Run("BookToBookIdOldBooksLogs", testBookToManyAddOpBookIdOldBooksLogs)
	t.Run("PersonToAuthorIdBooks", testPersonToManyAddOpAuthorIdBooks)
	t.Run("PersonToAuthorNewBooksLogs", testPersonToManyAddOpAuthorNewBooksLogs)
	t.Run("PersonToAuthorOldBooksLogs", testPersonToManyAddOpAuthorOldBooksLogs)
	t.Run("PersonToPeopleIdNewPeopleLogs", testPersonToManyAddOpPeopleIdNewPeopleLogs)
	t.Run("PersonToPeopleIdOldPeopleLogs", testPersonToManyAddOpPeopleIdOldPeopleLogs)
}

// TestToManySet tests cannot be run in parallel
// or deadlocks can occur.
func TestToManySet(t *testing.T) {
	t.Run("BookToBookIdNewBooksLogs", testBookToManySetOpBookIdNewBooksLogs)
	t.Run("BookToBookIdOldBooksLogs", testBookToManySetOpBookIdOldBooksLogs)
	t.Run("PersonToAuthorIdBooks", testPersonToManySetOpAuthorIdBooks)
	t.Run("PersonToAuthorNewBooksLogs", testPersonToManySetOpAuthorNewBooksLogs)
	t.Run("PersonToAuthorOldBooksLogs", testPersonToManySetOpAuthorOldBooksLogs)
	t.Run("PersonToPeopleIdNewPeopleLogs", testPersonToManySetOpPeopleIdNewPeopleLogs)
	t.Run("PersonToPeopleIdOldPeopleLogs", testPersonToManySetOpPeopleIdOldPeopleLogs)
}

// TestToManyRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestToManyRemove(t *testing.T) {
	t.Run("BookToBookIdNewBooksLogs", testBookToManyRemoveOpBookIdNewBooksLogs)
	t.Run("BookToBookIdOldBooksLogs", testBookToManyRemoveOpBookIdOldBooksLogs)
	t.Run("PersonToAuthorIdBooks", testPersonToManyRemoveOpAuthorIdBooks)
	t.Run("PersonToAuthorNewBooksLogs", testPersonToManyRemoveOpAuthorNewBooksLogs)
	t.Run("PersonToAuthorOldBooksLogs", testPersonToManyRemoveOpAuthorOldBooksLogs)
	t.Run("PersonToPeopleIdNewPeopleLogs", testPersonToManyRemoveOpPeopleIdNewPeopleLogs)
	t.Run("PersonToPeopleIdOldPeopleLogs", testPersonToManyRemoveOpPeopleIdOldPeopleLogs)
}

func TestReload(t *testing.T) {
	t.Run("Books", testBooksReload)
	t.Run("BooksLogs", testBooksLogsReload)
	t.Run("People", testPeopleReload)
	t.Run("PeopleLogs", testPeopleLogsReload)
}

func TestReloadAll(t *testing.T) {
	t.Run("Books", testBooksReloadAll)
	t.Run("BooksLogs", testBooksLogsReloadAll)
	t.Run("People", testPeopleReloadAll)
	t.Run("PeopleLogs", testPeopleLogsReloadAll)
}

func TestSelect(t *testing.T) {
	t.Run("Books", testBooksSelect)
	t.Run("BooksLogs", testBooksLogsSelect)
	t.Run("People", testPeopleSelect)
	t.Run("PeopleLogs", testPeopleLogsSelect)
}

func TestUpdate(t *testing.T) {
	t.Run("Books", testBooksUpdate)
	t.Run("BooksLogs", testBooksLogsUpdate)
	t.Run("People", testPeopleUpdate)
	t.Run("PeopleLogs", testPeopleLogsUpdate)
}

func TestSliceUpdateAll(t *testing.T) {
	t.Run("Books", testBooksSliceUpdateAll)
	t.Run("BooksLogs", testBooksLogsSliceUpdateAll)
	t.Run("People", testPeopleSliceUpdateAll)
	t.Run("PeopleLogs", testPeopleLogsSliceUpdateAll)
}
