#!/bin/bash

if [[ $# -eq 0 ]]
then
	echo "Please provide version number."
	exit 1
fi

VERSION="$1"
TEMP=$(mktemp)
TODAY=$(date '+%Y.%m.%d')

# source
cat "src/Main.hs" | sed "s:appVersion *=.*\$:appVersion = \"$VERSION\":" > "$TEMP"
cp $TEMP "src/Main.hs"

# cabal
cat "pictikz.cabal" | sed "s/^version:\( *\)[\"0-9\\.]*/version:\1$VERSION/" > "$TEMP"
cp $TEMP "pictikz.cabal"

# Changelog
echo "# Revision history for pictikz
" > "$TEMP"
echo "## $1 -- $TODAY" >> "$TEMP"
tail -n +3 ChangeLog.md >> "$TEMP"
cp "$TEMP" ChangeLog.md

# man page
cd docs
cat man.header | sed 's:\.TH\([^"]*\)"[^"]*".*\("[^"]*man page"\):.TH\1"'"$TODAY"'" "'"$VERSION"'" \2:' > "$TEMP"
cp "$TEMP" man.header

make
cp pictikz.1 ../
cp README.md ../
