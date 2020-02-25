import re

rx = re.compile("{([^}]+)}")
fmt = "/topics/{ topicId }/topic-comments/{topicCommentId}:"
for placeholder in rx.findall(fmt):
    print(placeholder.split())

