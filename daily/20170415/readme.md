# yaml prefixとして使える文字

- $

テキトウにyamlのコード見るのが早そう。

```
# yaml/scanner.py
class Scanner:

    def check_plain(self):

        # A plain scalar may start with any non-space character except:
        #   '-', '?', ':', ',', '[', ']', '{', '}',
        #   '#', '&', '*', '!', '|', '>', '\'', '\"',
        #   '%', '@', '`'.
        #
        # It may also start with
        #   '-', '?', ':'
        # if it is followed by a non-space character.
        #
        # Note that we limit the last rule to the block context (except the
        # '-' character) because we want the flow context to be space
        # independent.
        ch = self.peek()
        return ch not in '\0 \t\r\n\x85\u2028\u2029-?:,[]{}#&*!|>\'\"%@`'  \
                or (self.peek(1) not in '\0 \t\r\n\x85\u2028\u2029'
                        and (ch == '-' or (not self.flow_level and ch in '?:')))
```
