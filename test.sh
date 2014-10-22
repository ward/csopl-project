#!/bin/bash

while read input; do
 	./Main <<< $input
 	echo "====================================================================="
done <test_input.txt
