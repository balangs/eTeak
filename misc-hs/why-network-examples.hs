import Report
import Control.Monad.Trans
import Network
import NetParts

a :: Why Int
a = do
	g <- Why Complete 20
	Why Complete (10 + g)

b :: Why Int
b = do
	g <- Why Complete 20
	Why (Wrong [Report NoPos "Error!"]) 'A'
	Why Complete (10 + g)

c = do
	i <- Why Complete (putStr "Hello")
	j <- Why Complete (putStr "World")
	return $ i >> j

d =	m
	where Why _ m = c

e = do
	WhyT $ do
		putStr "Hello"
		return $ Why Complete 0
	WhyT $ do
		putStr "World"
		return $ Why Complete 0

f = m
	where WhyT m = e

sameAsF = runWhyT e

g = do
	lift $ putStr "Hello"
	lift $ putStr "World"

h = m
	where WhyT m = g

link1 :: NetworkMonad Network ()
link1 = do
	nwNewLinkRef 0
	return ()

nw1 = snd $ runNetwork0 (link1 >> link1)

part1 = Part "a" [] nw1
