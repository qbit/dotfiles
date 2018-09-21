#!/bin/sh

if [[ -z $1 ]]; then
	mpc lsp | sort
else
  	mpc clear
	mpc load "$1"
	mpc toggle
fi
