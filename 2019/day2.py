from copy import deepcopy
program = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,5,19,23,1,6,23,27,1,27,10,31,1,31,5,35,2,10,35,39,1,9,39,43,1,43,5,47,1,47,6,51,2,51,6,55,1,13,55,59,2,6,59,63,1,63,5,67,2,10,67,71,1,9,71,75,1,75,13,79,1,10,79,83,2,83,13,87,1,87,6,91,1,5,91,95,2,95,9,99,1,5,99,103,1,103,6,107,2,107,13,111,1,111,10,115,2,10,115,119,1,9,119,123,1,123,9,127,1,13,127,131,2,10,131,135,1,135,5,139,1,2,139,143,1,143,5,0,99,2,0,14,0]


def apply_inputs(program, input1, input2):
    program[1] = input1
    program[2] = input2 
    return program 

def run(program, index):
    if program[index] == 99:
        return program
    if len(program[index:]) < 4:
        raise Exception("incorrect args") 
    arg1, arg2, output_index = program[program[index+1]],program[program[index+2]],program[index+3], 
    if program[index] == 1:
        program[output_index] = arg1 + arg2
        return run(program, index + 4)
    if program[index] == 2:
        program[output_index] = arg1 * arg2
        return run(program, index + 4)
    raise Exception("never halted") 

assert(run([2,3,0,3,99], 0) == [2,3,0,6,99])
assert(run([2,4,4,5,99,0], 0) == [2,4,4,5,99,9801])
assert(run([1,1,1,4,99,5,6,0,99], 0) == [30,1,1,4,2,5,6,0,99])

# Part 1
part1 = deepcopy(program)
print(run(apply_inputs(part1, 12, 2), 0))

# Part 2
inputs = [(i, j) for i in range(100) for j in range(100)]
current = 0
while True:
    testcase = deepcopy(program)
    a, b = inputs[current]
    run(apply_inputs(testcase, a, b), 0)
    print(a, b, testcase[0])
    if testcase[0] == 19690720:
        print(a, b, a*100 + b)
        break
    current += 1
