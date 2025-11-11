#!/bin/sh

set -xe

cd build/autoconf/

rm config.guess
curl -o config.guess 'https://gitweb.git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD'

rm config.sub
curl -o config.sub 'https://gitweb.git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD'

rm install-sh
curl -o install-sh 'https://gitweb.git.savannah.gnu.org/gitweb/?p=automake.git;a=blob_plain;f=lib/install-sh;hb=HEAD'
