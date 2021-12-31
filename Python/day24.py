#!python3

# https://gist.github.com/jkseppan/1e36172ad4f924a8f86a920e4b1dc1b1

# Magic constants derived from the ALU code
# These are the numbers from *Simon's* AoC input
As = [ 10, 12, 10, 12, 11,-16, 10,-11,-13, 13, -8, -1, -4,-14]
Bs = [ 12,  7,  8,  8, 15, 12,  8, 13,  3, 13,  3,  9,  4, 13]
Cs = [  1,  1,  1,  1,  1, 26,  1, 26, 26,  1, 26, 26, 26, 26]

# to get these, I split the input by "inp w" lines
# and diffed the blocks: the A is the varying x addend,
# the B the varying y addend and the C the varying z divisor
# (apparently always 1 or 26 depending on the sign of A)

import itertools as it

# testing tools (not needed for the actual solution)

def forward(A, B, C, z, w):
    "this is what a single block does"
    z1 = z//C
    if w == z % 26 + A:
        return z1
    else:
        return 26 * z1 + w + B

def forward_all(ws,As=As,Bs=Bs,Cs=Cs,z=0):
    "the whole program"
    for A,B,C,w in zip(As,Bs,Cs,ws):
        z = forward(A,B,C,z,w)
    return z

# solution starts here
  
def backward(A, B, C, z2, w):
    """The possible values of z before a single block
    if the final value of z is z2 and w is w"""
    zs = []
    x = z2 - w - B
    if x % 26 == 0:
        zs.append(x//26 * C)
    if 0 <= w-A < 26:
        z0 = z2 * C
        zs.append(w-A+z0)
    
    #print(f'backward({A},{B},{C},{z2},{w}) returned " + {zs}')
    return zs

def solve(part,As=As,Bs=Bs,Cs=Cs):
    zs = {0}
    result = {}
    if part == 1:
        ws = range(1,10)
    else:
        ws = range(9,0,-1)
    for A,B,C in zip(As[::-1],Bs[::-1],Cs[::-1]):
        #print(len(zs))
        newzs = set()
        for w,z in it.product(ws,zs):
            z0s = backward(A,B,C,z,w)
            for z0 in z0s:
                newzs.add(z0)
                result[z0] = (w,) + result.get(z, ())
        zs = newzs
    return ''.join(str(digit) for digit in result[0])

print(solve(1))
print(solve(2))