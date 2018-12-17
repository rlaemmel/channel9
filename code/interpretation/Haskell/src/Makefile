test:
	make 0-ConstAdd.test
	make 1-ZeroSucc.test
	make 2-ZeroSuccPred.test
	make 3-NB.test
	make 4-Lambda.test
	make 5-Letrec.test
	make 6-Monads.test
	make 7-Bananas.test

%.test:
	make $*.cpp
	echo "main" | ghci -v0 -Wall -i../include cache/$*.hs > log
	diff log $*/baseline
	rm -f log

%.ghci: 
	make $*.cpp
	ghci -Wall -i../include cache/$*.hs

%.cpp:
	cpp -P -I$* Template.hs cache/$*.hs

clean:
	rm -f log
	rm -rf cache/*

