import math

angles = [90, 60, 0, 180, 210, -210, 1470, -1470, -4526]

def cos2(x):
	return math.cos(math.radians(x))

def sin2(x):
	return math.sin(math.radians(x))

for angle in angles:
	print("Sin: " + str(angle) + "| " + str(sin2(angle)))

for angle in angles:
	print("Cos: " + str(angle) + "| " + str(cos2(angle)))