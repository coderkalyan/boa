def fib(n: int):
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

print(fib(32))

# foo = 1
# def main():
#     local = foo
#
# main()
