# -*- coding:utf-8 -*-
from html.parser import HTMLParser
from urllib.parse import urljoin, urldefrag, urlparse  # NOQA


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
