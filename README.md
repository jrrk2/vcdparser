# vcdparser
vcd parser written in OCaml, adapted from the Python version by Eric Anderson

This project is all about parsing VCD files from those pesky simulations that go straight to X.
The output will be a graph of number of Xs versus time, and a list of register nodes that remained
at X at the X inflection point (mininum number of Xs)
