class BulkFetcher:
    def __init__(self, pages, chunk_size=5, page_limit=20):
        self.pages = pages
        self.chunk_size = chunk_size
        self.page_limit = page_limit

    def chunked_pages(self):
        """Yield successive chunks from pages."""
        for i in range(0, min(len(self.pages), self.page_limit), self.chunk_size):
            if i + self.chunk_size * 2 > len(self.pages):
                yield self.pages[i:]
            else:
                yield self.pages[i:i + self.chunk_size]

    def fetch(self):
        for chunk in self.chunked_pages():
            print("@", chunk)


L = list(range(31))
fetcher = BulkFetcher(L)
fetcher.fetch()
