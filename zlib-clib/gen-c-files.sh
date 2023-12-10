#!/bin/sh

set -xe

if [ -z "$1" ] ; then
	echo "No zlib version specified, aborting..."
	exit 1
fi

ZLIB_VER="$1"
ZLIB_BALL="zlib-${ZLIB_VER}.tar.gz"
ZLIB_URL="https://zlib.net/${ZLIB_BALL}"

[ -f "${ZLIB_BALL}" ] || curl -LO "${ZLIB_URL}"

[ -d "zlib-${ZLIB_VER}" ] || tar xf "${ZLIB_BALL}"

(
cd "zlib-${ZLIB_VER}"

rm -f ../c/*.[ch]
cp ./*.[ch] ../c/
)

rm -r "zlib-${ZLIB_VER}"
rm "${ZLIB_BALL}"

