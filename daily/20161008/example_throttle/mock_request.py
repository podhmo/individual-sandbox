class MockRequest:
    def __init__(self, domain, *args):
        self.domain = domain
        self.args = args
        self.links = []

    def add_link(self, request):
        self.links.append(request)
        return self

    def get_links(self):
        return self.links
