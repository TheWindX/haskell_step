--99 questions
import Control.Monad
import Data.Char
import System.IO
import System.Random
import Text.Printf
import Data.List
import System.Environment
import Debug.Trace


myLast :: [a]->a
myLast [a] = a
myLast (x:xs) = myLast xs

myLastButOne :: [a]->a
myLastButOne [] = error "none list!"
myLastButOne (x:[]) = error "one elem list!"
myLastButOne (x:xs) = if oneElement xs then
		x
	else
		myLastButOne xs


oneElement :: [a]->Bool
oneElement [] = False
oneElement (x:[]) = True
oneElement n = False


myLastButOneP :: [a]->a
myLastButOneP (x:(y:[]) ) = x
myLastButOneP (x:xs) = myLastButOneP xs


elemAt :: [a]->Integer->a
elemAt (x:xs) 1 = x
elemAt xs n = ((`elemAt` (n-1) ).tail) xs


myLength :: [a]->Int
myLength [] = 0

myLength xs = (+1).myLength.tail $ xs

myReverse [] = []
myReverse xs = (myReverse.tail $ xs) ++ [(head xs)]

isPalindrome :: (Eq a)=>[a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x==(last xs) ) && (isPalindrome.init $ xs)


data NestedList a = F a | N [NestedList a]

myFlat :: NestedList a ->  [a]

myFlat (F a) = [a]
myFlat (N [x] ) = myFlat x
myFlat (N (x:xs) ) = myFlat x ++ (myFlat (N xs) )






		
		
mySlice :: [a]->Int->Int->[a]
mySlice	arrIn m n = p2
	where
		(p1, p3) = splitAt m arrIn
		(p2, p4) = splitAt (n-m) p3
		
		
myRotate :: [a]->Int->[a]
myRotate arrIn n = visit arr1 arr2
	where
		(arr1, arr2) = splitAt (mod n $ length arrIn) arrIn
		visit a1 a2 = arr2++arr1
		
removeAt :: [a]->Int->[a]
removeAt arrIn n = arrOut
	where
		(arrOut, c) = 
			foldl 
			(\acc@(a, c) x->
				if (c+1) /=n then (a++[x], c+1) 
				else (a, c+1) ) 
			([], 0) arrIn
		

		
mrange m n = rh m n [] 
	where
		rh m n arr = if m<n then rh m (n-1) (n:arr) else (m:arr)



		
	
test = 
	let 
		t = (\c ->
				when (c /= "q") $ do
					putStrLn "wrong"
					c<-getLine
					t c
					)
	in
		do
			c<-getLine
			t c

test1 = forever $ do
	putStrLn "a word to translate"
	s <- getLine
	putStrLn $ map toUpper s
	
	
test2 = do
	s <- getContents
	putStrLn $ map toUpper s
	

test3 = 
		do
			let t = "test3.txt"
			f <- openFile t ReadMode
			c <- hGetContents f
			putStr $ "from file "++t++"\n"++c
		
test4 = 
	do
		let t gen = do 
			let (v, gen') = randomR (1, 3) gen 
			putStrLn $ printf "guess a num(%d):" v
			strv <- getLine
			let nv = (read strv) :: Int
			if (nv /= v) then
				t gen'
			else
				do
				putStrLn "good you make it"
				return ()

		gen <- getStdGen
		t gen
		
		

p23 :: [a]->Int->IO [a]
p23 arr num =
	do
		let len =  length arr
		g <- newStdGen
		let 
			h _ 0 arrOut = arrOut
			h g n arrOut = 
				let (r, g') = randomR (0, len-1) g
				in h g' (n-1) ((arr!!r):arrOut)
		return $ h g num []
			
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
	gen <- newStdGen
	let arr = randomRs (1, m) gen
	let takevn n arr@(x:xs) arrOut = 
		if n == 0 then arrOut
		else if x `elem` arrOut then takevn n xs arrOut
		else takevn (n-1) xs (x:arrOut)
	return $ takevn n arr []

shuffle :: [a]->IO [a]
shuffle [] = return []
shuffle [n] = return [n]
shuffle arr@(x:xs) = 
	do
		let 
			part::[a]->IO ([a], [a])
			part arr = 
				do
					r <- randomRIO (0, length arr)
					return $ splitAt r arr
		
		(p1, p2) <- part xs
		s1 <- (shuffle p1)
		s2 <- (shuffle p2)
		return $ s1++[x]++s2

		
		
--[(c, l)]
p26 n arr = 
	let 
		deep arrIn =
			let
				f x arr = filter (\n->n/=x) arr 
				s1 c l = [(c++[x], (f x l) ) | x<-l ]
				gather = foldl (\acc x@(c, l)->acc++(s1 c l) ) [] arrIn
			in gather
		deep' arrIn 0 = arrIn
		deep' arrIn acc = deep' (deep arrIn) (acc-1)
		ret = deep' [([], arr)] n
		ret' = [ c | (c, l)<-ret]
	in ret'
			
				
p261 n arr = 
	let
		deep :: [([a], [a])]->[([a], [a])] -- [( ns_elem, ele_left)]
		deep arrIn = 
			foldl 
				(\acc (c, l)->
					let
						iter acc n = 
							if n >= (length l) then acc
							else iter (acc++[((c++[l!!n]), drop (n+1) l)] ) (n+1)
					in iter acc 0
				) [] arrIn
		
		deepn 0 arrIn = arrIn
		deepn nn arrIn = deepn (nn-1) (deep arrIn)
		deepall = deepn n [([], arr)]
	in [c | (c, l) <- deepall]


mzip :: [a] -> [a] -> [(a,a)]
mzip []      ys      = []
mzip xs      []      = []
mzip (x:xs') (y:ys') = (x,y):mzip xs' ys'


myacc n = 
	let h ret iter 
			| iter==n = ret+n
			| otherwise = h (ret+iter) (iter+1)
	in h 0 0

myacc1 n = 
	if n== 0 then 0
	else myacc1 (n-1) + n
	
	
myacc2 n = f 0 n
   where f a 0 = a
         f a b = f (a+b) (b-1)
	


	

--main = printf $ show (myfoldl (+) 0 [1..1234567])
--main = printf $ show (myacc2 1234567)
--main = printf $ show (myacc 1234567)
--main = printf $ show (myacc3 1234567)


plist = filter p31 [2..]

pidx 2 = 0
pidx n = if p31 n then pidm (n-1)+1 else pidm (n-1)
pidm n = 
	let m = map pidx [2..]
	in m!!(n-2)

p31 :: Int -> Bool
p31 2 = True
p31 n = 
	let 
		plimit = pidm (n-1)
		tlist = take plimit plist
	in and $ map (\x->n `mod` x /= 0) tlist

	
	
-- pc
-- 
	
plist1 = 2:(myfilter 1 3)
	where
		myfilter pc iter = 
			if and $ map ((/=0).(mod iter) ) (take pc plist1)
			then iter:(myfilter (pc+1) (iter+1) )
			else (myfilter pc (iter+1) )


prims = 2:(checkfrom 3)
	where checkfrom iter =
		let pc = floor.sqrt.fromInteger $ iter
		in	if and $ map ((/=0).(mod iter) ) (take pc prims)
			then iter:(checkfrom (iter+2) )
			else (checkfrom (iter+2) )
		
--main = putStrLn $ show $ take 100 prims
				
				
				
isP n = isIn n prims
	where
		isIn n l@(x:xs) = if n==x then True
			else if n>x then isIn n xs
			else False
	


	
	
test310 :: (Eq a) => [a] -> Int -> [[a] ]
test310 arr n = 
	let 
		chooseone (c, l)
			| l==[] = []
			| otherwise = ((c++[head l]), tail l):(chooseone (c , tail l) )
		
		h (c, l) 0 = [(c,l)]
		h (c, l) n = concatMap chooseone (h (c, l) (n-1) )
		
		ret = h ([], arr) n
	in [c | (c, l)<-ret]
		
test323 :: [Int]->[Int]
test323 arr = 
	do 
		elem<-arr
		return elem

{- GDC -}
p32 :: Int->Int->Int

p32 m n =
	if m<n then p32 n m
	else
		let m' = mod m n
		in
			if m' == 0 then n
			else p32 n m'

p33 m n = 
	if p32 m n == 1 then True
	else False
	
p34 m =
	let 
		rets = [(x, p33 m x) | x<- [2..m] ]
	in	[ x| (x, ret)<-rets, ret]

p35 :: Integer -> [Integer]
p35 m =
	let 
		part m s = 
			let 
				f = filter ((==0).(mod m)) [s..m-1]
				h = head f
			in
				if f==[] then (1, m)
				else (h, div m h)
				
		iter m acc s = 
			let (p, l) = part m s
			in 
				if p == 1 then (l:acc)
				else iter l (p:acc) p
	in reverse $ iter m [] 2
	
		
p36 :: Integer -> [(Integer, Integer)]
p36 m = 
	let
		preRes@(x:xs) = p35 m
		facc = 
			\acc@(x, n, arr) e->
				if e==x then (x, n+1, arr)
				else (e, 1, (x, n):arr)
		r1@(x', n', arr') = foldl' facc (x, 1, []) xs
		r = (x', n'):arr'
	in r
		
		
		
--p40 :: Integer -> (Integer, Integer)
p40 n = 
	if not.even $ n then error "not a event number"
	else 
		let
			isP x = (==x).last $ (fst (span (<=x) prims) )
			l = (div n 2)
			ls = [n-x| x<- (fst (span (<l) prims) )]
			r1 = filter isP ls
			
			t = if r1==[] then error "exception!!!"
				else 
					let p1 = head r1
					in (p1, n-p1)
		in t
			
p41 :: Integer->Integer->[(Integer, Integer)]
p41 l u =
	let 
		es =  (filter even [l..u])
		ps = [ p40 e| e<-es]
		ps1 = [(p1, p2)|(p1, p2)<-ps, (abs(p1-p2)>=50), trace (show ("error", p1,p2)) True ]
	in ps1	
	
p46 :: (Bool->Bool->Bool) ->IO ()	
p46 f = 
	let
		t = [True, False]
		r = [(e1, e2, f e1 e2)|e1<-t, e2<-t]
	in putStrLn $ show $ r
	
{-class test-}
type Sheep = Int

sheepDbF = [(11,1),(12,1),(111,11)]
sheepDbM = [(16,6),(166,16),(11,6),(111,16)]

father n = lookup n sheepDbF
mother n = lookup n sheepDbM


class T a where
	str :: a->String
	id :: a->Int
	
instance T Int where
	str a ="inttype"
	id a = 1


data To = Tb | Ti | Ts | Tc Tcompound deriving (Show)
data Tcompound = Tand [To] | Tor [To] | TArrow {intype::To, outtype::To} | Tlambda [
data Var = T


