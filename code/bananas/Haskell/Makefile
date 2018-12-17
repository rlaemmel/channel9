none:

test:
	make Foldr.test
	make MapReduce.test
	make Large.test
	make SYB.test

%.test:
	ghci -v0 $*.hs -iinclude < tests/$*.input > tests/$*.output
	diff tests/$*.output tests/$*.baseline
	rm -f tests/$*.output
