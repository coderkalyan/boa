def main():
    hello = "Hello, "
    world = "world!"
    greeting = hello + world
    multiple = greeting * 3
    print(multiple)

    strlen = len(multiple)
    print(strlen)

    x = 0
    while x < 100_000:
        hello = "Hello, "
        world = "world!"
        greeting = hello + world
        multiple = greeting * 3

        strlen = len(multiple)
        x = x + 1

main()
