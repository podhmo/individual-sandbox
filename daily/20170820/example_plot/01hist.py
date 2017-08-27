import math
from utatane import as_command, subplot


def adjusted(xs):
    minv = min(xs)
    lsize = math.floor(math.log10(minv))
    left = (minv // (10 ** lsize)) * (10 ** lsize)

    maxv = max(xs)
    rsize = math.floor(math.log10(maxv))
    right = ((maxv // (10 ** rsize)) + 1) * (10 ** rsize)

    return left, right


@as_command
def render(plt):
    # 100人分の年齢
    ages = [
        23, 22, 23, 22, 24, 20, 22, 24, 29, 28, 25, 25, 26, 27, 28, 27, 25, 25, 27, 25, 25, 32, 32,
        32, 33, 33, 32, 33, 32, 30, 33, 32, 35, 39, 38, 38, 37, 35, 38, 35, 35, 38, 35, 37, 43, 44,
        40, 41, 44, 41, 40, 43, 44, 41, 41, 44, 43, 42, 40, 44, 42, 41, 42, 47, 49, 49, 46, 48, 45,
        49, 49, 49, 49, 49, 48, 46, 49, 45, 48, 49, 48, 54, 52, 54, 53, 53, 54, 50, 51, 52, 54, 58,
        56, 58, 58, 55, 57, 56, 56, 55
    ]
    with subplot(plt, ncols=2, nrows=1) as nth:
        with nth(1) as ax:
            # 8個の階級でヒストグラムを作成します。binsの最小値と最大値をrangeで指定します。
            # 戻り値について　n => 各階級における度数、bins => 階級のリスト
            n, bins, patches = ax.hist(ages, bins=8, range=(20, 60))

            # グラフのタイトル
            ax.set_title('Ages')
            # X軸のタイトル
            ax.set_xlabel('Age group')
            # Y軸のタイトル
            ax.set_ylabel('Numbers')

            # X軸のラベル
            label = [
                '20 - 24', '25 - 29', '30 - 34', '35 - 39', '40 - 44', '45 - 49', '50 - 54',
                '55 - 59'
            ]

            # ラベルを打つX軸の場所。階級値をセットする。
            ax.set_xticks([22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5])
            # X軸にラベルをセット、90度回転させる
            ax.set_xticklabels(label, rotation=90)

        with nth(2) as ax:
            left, right = adjusted(ages)
            n, bins, patches = ax.hist(ages, bins=range(left, right + 1, 5))
            print(bins)
            # グラフのタイトル
            ax.set_title('Ages')
            # X軸のタイトル
            ax.set_xlabel('Age group')
            # Y軸のタイトル
            ax.set_ylabel('Numbers')

            # ラベルを打つX軸の場所。階級値をセットする。
            ax.set_xticks([(x + y) / 2 for x, y in zip(bins, bins[1:])])
            # X軸にラベルをセット、90度回転させる
            ax.set_xticklabels(
                ["{} - {}".format(x, y - 1) for x, y in zip(bins, bins[1:])], rotation=90
            )
