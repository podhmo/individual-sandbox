# A simple script to calculate BMI
from pywebio.input import FLOAT, input
from pywebio.output import put_text, put_error


def bmi():
    height = input("Input your height(cm)：", type=FLOAT)
    weight = input("Input your weight(kg)：", type=FLOAT)

    BMI = weight / (height / 100) ** 2

    top_status = [
        (16, "Severely underweight"),
        (18.5, "Underweight"),
        (25, "Normal"),
        (30, "Overweight"),
        (35, "Moderately obese"),
        (float("inf"), "Severely obese"),
    ]

    for top, status in top_status:
        if BMI <= top:
            put_text("Your BMI: %.1f. Category: %s" % (BMI, status))
            break


def run():
    try:
        bmi()
    except Exception as e:
        put_error(e)
        import sys
        sys.excepthook(*sys.exc_info())


if __name__ == "__main__":
    run()
