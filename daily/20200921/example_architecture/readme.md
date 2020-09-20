## clean architecture

https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html

- Enterprise Business Rules

  - Entities

- Application Business Rules

  - Usecases (Interactors)

- Interface Adapter

  - Controllers
  - Gateways
  - Presenters

- Frameworks & Drivers

  - ui
  - web
  - devices
  - DB
  - External interfaces

## これをやる価値は？

- framework agnostic
- testable
- ui agnostic
- database agnostic
- loosely couping

hmm?

気にしたいのってportのパターンなきがする。

- web input, web output (request/response)
- cli input, cli output
- slack input, slack output

例えばこういう形態があった場合にいい感じにできるか。
framework agnosticは実装の詳細だし、database agnosticってそこまで気にしないのでは？
一番気にするのはui agnosticだと思う。

cli inputで考えると楽か。

