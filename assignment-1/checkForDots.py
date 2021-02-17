f = open("src/babies.sml", "r")
dotFound = False
ln = 1
for line in f:
	for ch in line:
		if(ch=='.'):
			dotFound = True
			print("Dot found in line " + str(ln))
	ln += 1
if(dotFound == False):
	print("No dots found!")

