#!/usr/bin/python

# room-parse.py
# Room Parser for sykosomatic
# Awfuly coded by Rick Rein <jeebusroxors@gmail.com>

# Input in XML:
#  <roomlist>
#     <room>
#        <name>This in the name</name>
#        <desc>description</desc>
#	 <long-desc>long description</long-desc>
#     </room>
#  </roomlist>

# Output to Lisp (ew):
#  (make-room :name "This is the name" :desc "desc" :desc-long "desc-long")

from xml.etree import cElementTree
import sys

def snuffalufigus():
	names = []
	descs = []
	longDescs = []
	output = file('room-parse-output','w')
	try:
		input = file(sys.argv[1])
	except:
		print "Somethings wrong with your input file"
		sys.exit(1)
	tree = cElementTree.parse(input)
	element = tree.getroot()
	values=[]
	for room in element.findall('room'):
        	for subElem in room:
			if subElem.tag == "name":
				names.append(subElem.text.strip())
			if subElem.tag == "desc":
				descs.append(subElem.text.strip())
			if subElem.tag == "long-desc":
				longDescs.append(subElem.text.strip())
	x=0
	if len(names) != len(longDescs):
		print "XML missing element"
		sys.exit(1)
	if len(names) != len(descs):
		print "XML missing element"
		sys.exit(1)

	while x < len(names):
		output.writelines('(make-room :name "' + names[x] + '" :desc "' + descs[x] + '" :desc-long "' + longDescs[x] + '")\n')
		x = x + 1

if __name__ == "__main__":
	snuffalufigus()
