from collections import defaultdict
from collections import deque


class BidirectionalBFS(object):
    def __init__(self, items):
        self.conv_map = defaultdict(list)
        for k, v in items:
            self.conv_map[k].append(v)
            self.conv_map[v].append(k)

    def resolve(self, src, dst):
        if src == dst:
            return []
        return self.tick_src([src], [dst], set(), deque(), deque())

    def on_finish(self, src_hist, dst_hist):
        src_hist.extend(reversed(dst_hist))
        path = src_hist
        coerce_path = []
        for i in range(len(path) - 1):
            coerce_path.append(["coerce", path[i], path[i + 1]])
        return coerce_path

    def tick_src(self, src_hist, dst_hist, arrived, src_q, dst_q):
        if src_hist[-1] == dst_hist[-1]:
            return self.on_finish(src_hist, dst_hist[:-1])
        if src_hist[-1] not in arrived:
            arrived.add(src_hist[-1])
            if src_hist[-1] in self.conv_map:
                for item in self.conv_map[src_hist[-1]]:
                    src_q.append(([*src_hist, item], dst_hist))
        if dst_q:
            src_hist, dst_hist = dst_q.pop()
            return self.tick_dst(src_hist, dst_hist, arrived, src_q, dst_q)
        elif src_q:
            src_hist, dst_hist = src_q.pop()
            return self.tick_src(src_hist, dst_hist, arrived, src_q, dst_q)
        else:
            return None

    def tick_dst(self, src_hist, dst_hist, arrived, src_q, dst_q):
        if src_hist[-1] == dst_hist[-1]:
            return self.on_finish(src_hist, dst_hist[:-1])
        if dst_hist[-1] not in arrived:
            arrived.add(dst_hist[-1])
            if dst_hist[-1] in self.conv_map:
                for item in self.conv_map[dst_hist[-1]]:
                    dst_q.append(([*dst_hist, item], dst_hist))
        if src_q:
            src_hist, dst_hist = src_q.pop()
            return self.tick_src(src_hist, dst_hist, arrived, src_q, dst_q)
        elif dst_q:
            src_hist, dst_hist = dst_q.pop()
            return self.tick_dst(src_hist, dst_hist, arrived, src_q, dst_q)
        else:
            return None
