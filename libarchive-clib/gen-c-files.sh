#!/bin/sh

set -xe

if [ -z "$1" ] ; then
	echo "No libarchive version specified, aborting..."
	exit 1
fi

LIBARCHIVE_VER="$1"
LIBARCHIVE_BALL="libarchive-${LIBARCHIVE_VER}.tar.gz"
LIBARCHIVE_URL="https://libarchive.org/downloads/${LIBARCHIVE_BALL}"

[ -f "${LIBARCHIVE_BALL}" ] || curl -LO "${LIBARCHIVE_URL}"

[ -d "libarchive-${LIBARCHIVE_VER}" ] || tar xf "${LIBARCHIVE_BALL}"

(
cd "libarchive-${LIBARCHIVE_VER}"

rm -f ../c/*.[ch]
cp libarchive/*.[ch] ../c/
)

rm -r "libarchive-${LIBARCHIVE_VER}"
rm "${LIBARCHIVE_BALL}"

