help:
	python -m monogusa.cli cli.py -h
init:
	python -m monogusa.cli cli.py init
add: TEXT ?= hello
add:
	python -m monogusa.cli cli.py add --text="${TEXT}"

list:
	python -m monogusa.cli cli.py list

ui-discord:
	python -m monogusa.chatbot.discordchat cli.py --name=notes
ui-slack:
	python -m monogusa.chatbot.slackchat cli.py --name=notes
