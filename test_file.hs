module Snippets where

import Data.List
import Data.List.Split
import System.IO
import Control.Exception
import Control.Monad

--simple console io
doubleEcho :: IO()
doubleEcho =
	do
		l <- getLine
		if l == "exit" then return ()
		else 
			do 
				putStrLn $ l++" "++l
				doubleEcho


--simple file io
changeLineEnd :: [Char]->[Char]
changeLineEnd sin = 
	let
		ls1 = splitWhen (\c->(c=='\n')||(c=='\r')) sin
		ls2 = filter (\x-> x /= "") ls1
		(hl:tls) = ls2
	in	foldl' (\acc elem->acc++('\n':elem) ) hl tls

--simple content in files translate
test1 fin fout = 
	do
		fih <- openFile fin ReadMode
		foh <- openFile fout WriteMode
		ci <- hGetContents fih
		let res = changeLineEnd ci
		hPutStr foh res
		hClose fih
		hClose foh

--exception catch
test2 fin =
	do
		--k <- catch (liftM Right $ openFile fin ReadMode) (\e-> return (Left $ show (e::IOException)) )
		k <- (try $ openFile fin ReadMode) -- try return a type with (Either e Handler)
		case k of
			Left e->putStrLn (show $ (e::IOException) )
			_->return ()


			