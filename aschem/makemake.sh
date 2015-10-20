HLDFLAGS="-package cairo"
HFLAGS="-O2 -Wall -fno-warn-name-shadowing @GHC_FLAGS@"
export HLDFLAGS
export HFLAGS

../bin/makemake aschem \
	--bindir noinst \
	Main.hs Patterns.hs Picture.hs Render.hs Types.hs > Makefile.am

cat >> Makefile.am <<EOF
EXTRA_DIST = makemake.sh
SUBDIRS = examples
EOF
