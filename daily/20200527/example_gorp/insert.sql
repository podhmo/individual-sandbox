-- https://eli.thegreenplace.net/2019/to-orm-or-not-to-orm/

CREATE TABLE IF NOT EXISTS Book (
   bookId INTEGER PRIMARY KEY AUTOINCREMENT,
   published DATE,
   title TEXT,
   url TEXT
);

CREATE TABLE IF NOT EXISTS Tag (
   tagId INTEGER PRIMARY KEY AUTOINCREMENT,
   name TEXT
);

CREATE TABLE IF NOT EXISTS Tag2Book (
   tagId INTEGER NOT NULL,
   bookId INTEGER NOT NULL,
   PRIMARY KEY (tagId, bookId),
   FOREIGN KEY (tagId) REFERENCES Tag(tagId)
   FOREIGN KEY (bookId) REFERENCES Book(bookId)
);

CREATE TABLE IF NOT EXISTS Comment (
   commentId INTEGER PRIMARY KEY AUTOINCREMENT,
   bookId INTEGER
   author TEXT,
   published DATE,
   content TEXT,
   FOREIGN KEY (bookId) REFERENCES Book(bookId)
);

-- https://github.com/dariubs/GoBooks

INSERT INTO Book(bookId, title, url) VALUES
  (1, 'The Little Go Book', 'http://openmymind.net/The-Little-Go-Book/'),
  (2, 'An Introduction to Programming in Go', 'http://www.golang-book.com/'),
  (3, 'Go Bootcamp', 'http://www.golangbootcamp.com/'),
  (4, 'Learning Go', 'http://www.miek.nl/go'),
  (5, 'Go for Javascript Developers', 'https://github.com/pazams/go-for-javascript-developers'),
  (6, 'Go in Action', 'https://www.manning.com/books/go-in-action'),
  (7, 'Go Programming Blueprints - 2nd Ed.', 'https://www.packtpub.com/application-development/go-programming-blueprints-second-edition'),
  (8, 'Programming in Go: Creating Applications for the 21st Century', 'http://www.informit.com/store/programming-in-go-creating-applications-for-the-21st-9780321774637'),
  (9, 'The Go Programming Language', 'http://gopl.io/'),
  (10, 'Introducing Go: Build Reliable, Scalable Programs', 'http://shop.oreilly.com/product/0636920046516.do'),
  (11, 'Get Programming with Go', 'https://www.manning.com/books/get-programming-with-go?a_aid=nathany&a_bid=53f68821'),
  (12, 'Go Programming by Example', 'https://www.amazon.com/Go-Programming-Example-Agus-Kurniawan-ebook/dp/B00TWLZVQQ'),
  (13, 'Go Recipes', 'http://www.apress.com/us/book/9781484211892'),
  (14, 'Learning Go programming', 'https://www.packtpub.com/application-development/learning-go-programming'),
  (15, 'API Foundations in Go', 'https://leanpub.com/api-foundations'),
  (16, 'Test-driven development with Go ', 'https://leanpub.com/golang-tdd'),
  (17, 'Go programming language secure coding practices guide', 'https://checkmarx.gitbooks.io/go-scp/'),
  (18, 'Network Programming with Go', 'https://www.apress.com/us/book/9781484226919'),
  (19, 'Mastering Concurrency in Go', 'http://shop.oreilly.com/product/9781783983483.do'),
  (20, 'Go in Practice', 'http://www.manning.com/butcher/'),
  (21, 'A Go Developer''s Notebook', 'https://leanpub.com/GoNotebook/'),
  (22, 'The Go Programming Language Phrasebook', 'http://www.informit.com/store/go-programming-language-phrasebook-9780321817143'),
  (23, 'Go Design Patterns', 'https://www.packtpub.com/application-development/go-design-patterns'),
  (24, 'Black Hat Go', 'https://www.nostarch.com/blackhatgo'),
  (25, 'Concurrency in Go', 'http://shop.oreilly.com/product/0636920046189.do'),
  (26, 'Hands-On Dependency Injection in Go', 'https://amzn.to/2Q6dLQC'),
  (27, 'Building Web Apps with Go', 'https://www.gitbook.com/book/codegangsta/building-web-apps-with-go/details'),
  (28, 'Build Web Application with Golang', 'https://www.gitbook.com/book/astaxie/build-web-application-with-golang/details'),
  (29, 'Webapps in Go the anti textbook', 'https://github.com/thewhitetulip/web-dev-golang-anti-textbook'),
  (30, 'Mastering Go Web Services ', 'http://shop.oreilly.com/product/9781783981304.do'),
  (31, 'Level Up Your Web Apps With Go', 'https://learnable.com/books/level-up-your-web-apps-with-go'),
  (32, 'Go Web Programming', 'http://www.manning.com/chang/'),
  (33, 'Cloud Native Go: Building Web Applications and Microservices for the Cloud with Go and React', 'https://www.amazon.com/Cloud-Native-Applications-Microservices-Developers/dp/0672337797'),
  (34, 'Web Development with Go: Learn to Create Real World Web Applications using Go', 'https://gumroad.com/l/web-development-with-go'),
  (35, 'Go: Building Web Applications', 'https://amzn.com/B01LD8K5C0'),
  (36, 'Building Microservices with Go', 'https://www.packtpub.com/application-development/building-microservices-go'),
  (37, '12 Factor Applications with Docker and Go', 'https://leanpub.com/12fa-docker-golang'),
  (38, 'Build SaaS apps in Go', 'https://buildsaasappingo.com'),
  (39, 'Let''s Go!', 'https://lets-go.alexedwards.net/')
  -- (40, 'A tour of Go', 'https://tour.golang.org/'),
  -- (41, 'Video: Learn Go Syntax in one video', 'http://www.youtube.com/watch?v=CF9S4QZuV30'),
  -- (42, 'Tutorials: Go by Example', 'https://gobyexample.com/'),
  -- (43, 'Go Fundamentals Video Training', 'http://shop.oreilly.com/category/learning-path/go-fundamentals.do'),
  -- (44, 'More Books on the Go Wiki', 'https://github.com/golang/go/wiki/Books'),
  -- (45, 'TutorialEdge.net Course', 'https://tutorialedge.net/course/golang/'),
  -- (46, 'Coursera Specialization: Programming with Go', 'https://www.coursera.org/specializations/google-golang/'),
  -- (47, 'Course: Mastering Go Programming', 'https://www.udemy.com/course/mastering-go-programming')
;

INSERT INTO Tag(tagId, name) VALUES
  (1, 'free'),
  (2, 'starter'),
  (3, 'adbanced'),
  (4, 'web')
;

INSERT INTO Tag2Book(tagId, bookId) VALUES
  (1, 1),
  (1, 2),
  (1, 3),
  (1, 4),
  (1, 5),
  (1, 16),
  (1, 17),
  (1, 27),
  (1, 28),
  (1, 29),
  (2, 1),
  (2, 2),
  (2, 3),
  (2, 4),
  (2, 5),
  (2, 6),
  (2, 7),
  (2, 8),
  (2, 9),
  (2, 10),
  (2, 11),
  (2, 12),
  (2, 13),
  (2, 14),
  (2, 15),
  (3, 16),
  (3, 17),
  (3, 18),
  (3, 19),
  (3, 20),
  (3, 21),
  (3, 22),
  (3, 23),
  (3, 24),
  (3, 25),
  (3, 26),
  (4, 27),
  (4, 28),
  (4, 29),
  (4, 30),
  (4, 31),
  (4, 32),
  (4, 33),
  (4, 34),
  (4, 35),
  (4, 36),
  (4, 37),
  (4, 38),
  (4, 39)
;

INSERT INTO Comment (bookId, content) VALUES
  (1, 'The Little Go Book is a free introduction to Google''s Go programming language. It''s aimed at developers who might not be quite comfortable with the idea of pointers and static typing. It''s longer than the other Little books, but hopefully still captures that little feeling.'),
  (2, 'This book is a short, concise introduction to computer programming using the language Go. Designed by Google, Go is a general purpose programming language with modern features, clean syntax and a robust well-documented common library, making it an ideal language to learn as your first programming language.'),
  (3, 'This companion book contains material initially written specifically for this event as well as content from Google & the Go team under Creative Commons Attribution 3.0 License and code licensed under a BSD license.')
;
