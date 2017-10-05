def fizzbuzz(num):
    if num % 15 == 0:
        print("FizzBuzz")
    if num % 15 != 0 and num % 5 == 0:
        print("Buzz")
    if num % 15 != 0 and num % 3 == 0:
        print("Fizz")
    if num % 5 != 0 and num % 3 != 0:
        print(num)

for i in range(20):
    fizzbuzz(i)
