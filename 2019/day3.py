import sys
import math

class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return "({}, {})".format(self.x, self.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def manhattan(self):
        # to origin
        return abs(self.x) + abs(self.y)

class Line(object):
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def length(self):
        return math.sqrt(math.pow(self.p1.x - self.p2.x, 2) + math.pow(self.p1.y - self.p2.y, 2)) 

    def horizontal(self):
        return self.p1.y == self.p2.y

    def _intersect(self, h, v):
        # check if horizontal line h intersects vertical line v
        if (h.p1.x <= v.p1.x <= h.p2.x or h.p1.x >= v.p1.x >= h.p2.x) and \
           (v.p1.y <= h.p1.y <= v.p2.y or v.p1.y >= h.p1.y >= v.p2.y):
            return (Point(v.p1.x, h.p1.y), True)
        return (None, False)

    def intersect(self, other):
        if (self.horizontal() and other.horizontal()) or \
            (not self.horizontal() and not other.horizontal()):
            return (None, False)
        if self.horizontal():
            return self._intersect(self, other)
        return self._intersect(other, self)

    def __repr__(self):
        return "{} -> {}".format(str(self.p1), str(self.p2))


l1 = Line(Point(0, 0), Point(10, 10))
l2 = Line(Point(-5, 5), Point(5, 5))
assert(l1.intersect(l2) == (Point(0, 5), True))
assert(l2.intersect(l1) == (Point(0, 5), True))

def steps_to_lines(steps):
    # Given a list of instructions construct a set of line segments
    lines = []
    curr = Point(0, 0) 
    for step in steps:
        start = Point(curr.x, curr.y)
        direction = step[0]
        amount = int(step[1:])
        if direction == 'R':
            end = Point(start.x + amount, start.y)
        elif direction == 'L':
            end = Point(start.x - amount, start.y)
        elif direction == 'U':
            end = Point(start.x, start.y + amount)
        elif direction == 'D':
            end = Point(start.x, start.y - amount)
        curr = end
        lines.append(Line(start, end))
    return lines


def closest(wire1, wire2):
    closest = sys.maxsize
    intersections = []
    for (i, s1) in enumerate(wire1):
        for (j, s2) in enumerate(wire2):
            p, intersect = s1.intersect(s2)
            if intersect and not (p.x == 0 and p.y == 0) and p.manhattan() < closest:
                intersections.append((p, i, j))
                closest = p.manhattan() 
    return (closest, intersections)
            
test1 = (['R75', 'D30', 'R83', 'U83', 'L12', 'D49', 'R71', 'U7', 'L72'], ['U62', 'R66', 'U55', 'R34', 'D71', 'R55', 'D58', 'R83'])
assert(closest(steps_to_lines(test1[0]),
               steps_to_lines(test1[1]))[0] == 159)
# Part 1
with open('input_day3.txt') as f:
    w1 = f.readline().rstrip('\n').split(',')
    w2 = f.readline().rstrip('\n').split(',')
    lines1 = steps_to_lines(w1)
    lines2 = steps_to_lines(w2)

closest_intersection, intersections = closest(lines1, lines2)
print(closest_intersection)

# Part 2
def compute_steps(lines, intersection_point):
    total = 0
    for l in lines[:-1]: 
        total += l.length()
    # The last line is the one that intersects
    # only add part of it
    if lines[-1].horizontal():
        # intersects at the x coord of the point 
        total += (intersection_point.x - lines[-1].p1.x)
    else:
        total += (intersection_point.y - lines[-1].p1.y)
    return total

best = sys.maxint
for point, line1, line2 in intersections:
    # Pair of lines which intersect
    w1_steps = compute_steps(lines1[:line1+1], point) 
    w2_steps = compute_steps(lines2[:line2+1], point) 
    total = w1_steps + w2_steps
    if total < best:
        best = total
print(int(best))
     



