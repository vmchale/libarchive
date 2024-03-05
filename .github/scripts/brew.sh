#!/bin/sh

set -eux

if [ -e "$HOME/.brew" ] ; then
	(
	cd "$HOME/.brew"
	git fetch --depth 1
	git reset --hard origin/master
	)
else
	git clone --depth=1 https://github.com/Homebrew/brew "$HOME/.brew"
fi
export PATH="$HOME/.brew/bin:$HOME/.brew/sbin:$PATH"

mkdir -p $GITHUB_WORKSPACE/.brew_cache
export HOMEBREW_CACHE=$GITHUB_WORKSPACE/.brew_cache
mkdir -p $GITHUB_WORKSPACE/.brew_logs
export HOMEBREW_LOGS=$GITHUB_WORKSPACE/.brew_logs
mkdir -p /private/tmp/.brew_tmp
export HOMEBREW_TEMP=/private/tmp/.brew_tmp

brew update
brew install ${1+"$@"}

