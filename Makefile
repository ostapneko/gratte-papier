.DEFAULT_GOAL := install

install: gratte-papier.cabal
	cabal install

gratte-papier.cabal: gratte-papier.cabal.m4
	m4 gratte-papier.cabal.m4 > gratte-papier.cabal
