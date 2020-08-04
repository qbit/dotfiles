#!/bin/sh

# Update a git repo that has a "upstream" remote

msg() {
	echo "==> ${1}"
}

clear

msg "Updating ${PWD}"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
if git remote | grep -q upstream; then
	msg "Found upstream"
	if [ "${BRANCH}" = "master" ]; then
		msg "Pulling upstream -> master"
		git pull upstream master
	else
		msg "Fetching all"
		git fetch --all
		msg "Don't forget to merge or pull!"
	fi
else
	msg "Origin is upstream"
	if [ "${BRANCH}" = "master" ]; then
		msg "Pulling from origin:master"
		git pull
	else
		msg "Fetching all"
		git fetch --all
		msg "Don't forget to merge or pull!"
	fi
fi
