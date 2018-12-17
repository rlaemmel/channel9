test: tests/output.txt tests/baseline.txt
	diff tests/output.txt tests/baseline.txt

tests/output.txt: *.hs Makefile tests/input.txt
	ghci -v0 Main.hs < tests/input.txt > tests/output.txt

clean:
	rm -f tests/output.txt
