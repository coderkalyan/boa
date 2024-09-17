x = 1 # look, I'm an integer

index = 0
while index < 10:
    print(x) # what is the type of this?
    x = "Hello, world!" # haha sucker, I'm a string now
    index += 1

types = [int, float, bool, str, dict, set]
x = None
for t in types:
    print(x)
    x = t()
