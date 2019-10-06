#!/bin/bash

elm make src/Main.elm --debug
echo -e '\033[32mSuccesfully compiled elm files\033[0m'

while true; do
	inotifywait -e modify -e create -e delete -r src
	elm make src/Main.elm --debug
	if [ $? -eq 0 ]; then
		echo -e '\033[32mSuccesfully compiled elm files\033[0m'
	fi
done