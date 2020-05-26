broken

```
# m/models [m/models.test]
./books.go:411:6: type Book has both field and method named AuthorId
./booksLog.go:421:6: type BooksLog has both field and method named AuthorNew
./booksLog.go:435:6: type BooksLog has both field and method named AuthorOld
./booksLog.go:449:6: type BooksLog has both field and method named BookIdNew
./booksLog.go:463:6: type BooksLog has both field and method named BookIdOld
./peopleLog.go:402:6: type PeopleLog has both field and method named PeopleIdNew
./peopleLog.go:416:6: type PeopleLog has both field and method named PeopleIdOld
FAIL    m/models [build failed]
```
