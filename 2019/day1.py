# Part 1
total = 0
with open('input_day1.txt') as f:
    for line in f:
        total += (int(line) / 3 - 2)
print(total)

# Part 2
def total_fuel(fuel):
    if fuel <= 0:
        return 0
    return fuel + total_fuel(fuel / 3 - 2)

assert(total_fuel(14 / 3 - 2) == 2)
assert(total_fuel(1969 / 3 - 2) == 966)
assert(total_fuel(100756 / 3 - 2) == 50346)

total = 0
with open('input_day1.txt') as f:
    for line in f:
        total += total_fuel(int(line) / 3 - 2) 
print(total)

