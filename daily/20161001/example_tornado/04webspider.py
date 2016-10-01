#!/usr/bin/env python

import time
from datetime import timedelta

try:
    from HTMLParser import HTMLParser
    from urlparse import urljoin, urldefrag
except ImportError:
    from html.parser import HTMLParser
    from urllib.parse import urljoin, urldefrag

from tornado import httpclient, gen, ioloop, queues

base_url = 'http://www.tornadoweb.org/en/stable/'
concurrency = 10


@gen.coroutine
def get_links_from_url(uid, url):
    """Download the page at `url` and parse it for links.

    Returned links have had the fragment after `#` removed, and have been made
    absolute so, e.g. the URL 'gen.html#tornado.gen.coroutine' becomes
    'http://www.tornadoweb.org/en/stable/gen.html'.
    """
    try:
        response = yield httpclient.AsyncHTTPClient().fetch(url)
        print('fetched(uid={}) {}'.format(uid, url))

        html = response.body if isinstance(response.body, str) else response.body.decode()
        urls = [urljoin(url, remove_fragment(new_url))
                for new_url in get_links(html)]
    except Exception as e:
        print('Exception(uid={}): {} {}'.format(uid, e, url))
        raise gen.Return([])

    raise gen.Return(urls)


def remove_fragment(url):
    pure_url, frag = urldefrag(url)
    return pure_url


class URLSeeker(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.urls = []

    def handle_starttag(self, tag, attrs):
        href = dict(attrs).get('href')
        if href and tag == 'a':
            self.urls.append(href)


def get_links(html):
    url_seeker = URLSeeker()
    url_seeker.feed(html)
    return url_seeker.urls


@gen.coroutine
def main():
    q = queues.Queue()
    start = time.time()
    fetching, fetched = set(), set()

    @gen.coroutine
    def fetch_url(uid):
        current_url = yield q.get()
        try:
            if current_url in fetching:
                return

            print('fetching(uid={}) {}'.format(uid, current_url))
            fetching.add(current_url)
            urls = yield get_links_from_url(uid, current_url)
            fetched.add(current_url)

            for new_url in urls:
                # Only follow links beneath the base URL
                if new_url.startswith(base_url):
                    yield q.put(new_url)

        finally:
            q.task_done()

    @gen.coroutine
    def worker(uid):
        while True:
            yield fetch_url(uid)

    q.put(base_url)

    # Start workers, then wait for the work queue to be empty.
    for i in range(concurrency):
        worker(i)
    yield q.join(timeout=timedelta(seconds=300))
    assert fetching == fetched
    print('Done in {} seconds, fetched {} URLs.'.format() % (time.time() - start, len(fetched)))


if __name__ == '__main__':
    import logging
    logging.basicConfig()
    st = time.time()
    io_loop = ioloop.IOLoop.current()
    io_loop.run_sync(main)
