from datetime import timedelta, datetime

# 5時間
# 15分毎のデータ
# 適当な範囲でグルーピング

now = datetime.now()
yesterday = now + timedelta(seconds=-60 * 60 * 5)

dt = yesterday
points = []
v = 0
while dt < now:
    dt += timedelta(seconds=900)
    v += 15
    points.append((dt, v))

name = "xxx"
for t, v in points:
    print(
        f"INSERT {name},type=line value={v} {int(t.timestamp()) * 10 ** 9}"
    )
