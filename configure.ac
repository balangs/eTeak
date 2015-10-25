dnl Process this file with autoconf to produce a configure script.

# Initialisations
AC_INIT([teak], [0.4])
AC_CONFIG_SRCDIR(library/teak/builtin.balsa)
AM_INIT_AUTOMAKE([foreign])

AC_PROG_INSTALL
AC_PATH_PROG(GHC, ghc, no)

if test ${GHC} = no; then
	AC_MSG_ERROR(ghc (The Glasgow Haskell Compiler) must be installed to build Teak)
fi

PACKAGES=

HAS_LINE_EDITING=0
# AC_MSG_CHECKING(for line editing in GHC)
# cat > lineedit.hs <<EOF
# module Main where
# 	import System.Console.Editline.Readline as Readline
# 
# 	main = putStrLn "OK"
# EOF
# 
# if test -x "${GHC}" && ${GHC} -o lineedit.test -package editline lineedit.hs > /dev/null 2> /dev/null; then
# 	AC_MSG_RESULT(yes)
# 	HAS_LINE_EDITING=1
# 	PACKAGES="${PACKAGES} -package editline"
# else
# 	AC_MSG_RESULT(no)
# fi
# rm -f lineedit.hs lineedit.hi lineedit.o lineedit.test

HAS_GTK2HS=0
AC_MSG_CHECKING(for gtk2hs/cairo in GHC)
cat > gtk2hs.hs <<EOF
module Main where
	import Graphics.UI.Gtk
	import Graphics.Rendering.Cairo
	import Graphics.UI.Gtk.Gdk.EventM

	main = putStrLn "OK"
EOF

GTK2HS_PACKAGE=
if test -x "${GHC}" && ${GHC} -o gtk2hs.test -package gtk -package cairo gtk2hs.hs > /dev/null 2> /dev/null; then
	AC_MSG_RESULT(yes)
	HAS_GTK2HS=1
	PACKAGES="${PACKAGES} -package gtk -package cairo"
else
	AC_MSG_RESULT(no)
	AC_MSG_ERROR(gtk2hs (Haskell bindings for GTK+/cairo) must be installed to build Teak)
fi
rm -f gtk2hs.hs gtk2hs.hi gtk2hs.o gtk2hs.test
AC_SUBST(HAS_GTK2HS)

GHC_FLAGS=
AC_MSG_CHECKING(for allowable warning flags for ghc)
cat > ghctest.hs <<EOF
module Main where
	main = putStrLn "OK"
EOF
if test -x "${GHC}" && ${GHC} -fno-warn-unused-do-bind -o ghctest.test ghctest.hs > /dev/null 2> /dev/null; then
	GHC_FLAGS="${GHC_FLAGS} -fno-warn-unused-do-bind"
fi
AC_MSG_RESULT(${GHC_FLAGS})
rm -f ghctest.hs ghctest.hi ghctest.o ghctest.test
AC_SUBST(GHC_FLAGS)

AC_SUBST(PACKAGES)

cat > src/Config.hs <<EOF
module Config (
	buildTeakHome,
	teakVersion
	) where

EOF

# if test ${HAS_LINE_EDITING} = 1; then
#	cat >> src/Config.hs <<EOF
#	import qualified System.Console.Editline.Readline as Readline
#
#	readlineInitialize = Readline.initialize
#	readline = Readline.readline
#	setCompletionEntryFunction = Readline.setCompletionEntryFunction
#	addHistory = Readline.addHistory
# EOF
# else
# 	changequote(<q<,>q>)dnl
# 	cat >> src/Config.hs <<EOF
# 	import Monad
# 
# 	readline :: String -> IO (Maybe String)
# 	readline prompt = do
# 		putStr prompt
# 		hFlush stdout
# 		liftM Just getLine
# 
# 	readlineInitialize :: IO ()
# 	readlineInitialize = return ()
# 
# 	setCompletionEntryFunction :: Maybe (String -> IO [String]) -> IO ()
# 	setCompletionEntryFunction _ = return ()
# 
# 	addHistory :: String -> IO ()
# 	addHistory _ = return ()
# EOF
# 	changequote([,])dnl
# fi

HAS_GUI=False
if test ${HAS_GTK2HS} = 1; then
	HAS_GUI=True
fi

# Make sure ${prefix} is set
test "x$prefix" = xNONE && prefix=$ac_default_prefix

cat >> src/Config.hs <<EOF
	buildTeakHome :: String
	buildTeakHome = "${prefix}"

	teakVersion :: String
	teakVersion = "${VERSION}"
EOF

AC_PATH_PROG(CONVERT_GM, gm, no)
if test ${CONVERT_GM} = no; then
	AC_PATH_PROG(CONVERT, convert, no)
else
	CONVERT="${CONVERT_GM} convert"
fi
AC_SUBST(CONVERT)

AC_SUBST(librarydir, ${prefix}/share/teak/library)
AC_SUBST(runtimedir, ${prefix}/share/teak/runtime)
AC_SUBST(techdir, ${prefix}/share/teak/tech)
AC_SUBST(optimdir, ${prefix}/share/teak/optim)

AC_OUTPUT([Makefile
	bin/Makefile
	bin/teak-config
	src/Makefile
	doc/Makefile
	doc/manual/Makefile
	library/Makefile
	library/teak/Makefile
	library/teak/sim/Makefile
	library/balsa/Makefile
	library/balsa/types/Makefile
	library/balsa/sim/Makefile
	library/balsa/parts/Makefile
	runtime/Makefile
	runtime/verilog/Makefile
	tech/Makefile
	optim/Makefile
	aschem/examples/Makefile
	aschem/Makefile
	examples/Makefile
	examples/sparkler/Makefile
	examples/builtins/Makefile
	examples/events/Makefile
	examples/ssem/Makefile
	examples/punf/Makefile
	examples/small/Makefile
	misc-hs/Makefile
	misc-hs/traverse/Makefile
	])
