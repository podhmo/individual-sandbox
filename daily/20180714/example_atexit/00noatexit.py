try:
    print("start")
    raise Exception("oops")
    print("end")
finally:
    print("finally")
