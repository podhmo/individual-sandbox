def fizzbuzz(num):
    if num % 15 == 0:
        print("FizzBuzz")
    else:
        if num % 5 == 0:
            print("Buzz")
        else:
            if num % 3 == 0:
                print("Fizz")
            else:
                print(num)


for i in range(20):
    fizzbuzz(i)
