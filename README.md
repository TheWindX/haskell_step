我希望这是一个haskell大纲和重点，目的是如果我很长时间没使用过HASKELL，那么跟随这个我能够很快找回陌生的概念（我过去学习过haskell的体会)，随着学习进程，这篇内容可以继续下去，丰富，变成一个对所有人都有用的教程。

other reference:
------------------------------------
(http://www.haskell.org/wikiupload/b/b2/QuickReference.pdf)
(http://shuklan.com/haskell/lec01.html#/)//Introduction To Haskell;Lecture 1;An Unexpected Journey
(http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/)//Learn Haskell Fast and Hard
(http://cheatsheet.codeslower.com/CheatSheet.pdf)//

haskell 的程序结构：
------------------------------------
--save as first.hs
{-
compile as: ghc --make first.hs -O2 -o first
run as: first
-}
module Main where
main = putStrLn "my first"  

模块：
------------------------------------
	--use keyword
	--import, module, qualified, as, hiding


	import Data.List
	import Data.List (sort)
	import Data.List hiding (nub)
	import qualified Data.Map --这样引用模块名，并带入名称限定，比如说可使用Data.Map.filter
	import qualified Data.Map as M


	module Geometry.Sphere  
	( volume  
	, area  
	) where

语法：
--------------------------------------
	缩进:
		(http://en.wikibooks.org/wiki/Haskell/Indentation)
		原则一点就是如果是语法结构的一部分进行缩进，同级的语法结构对齐
		缩进可用 {} 替代，同级的结构可用 ;分开，这样可将语法写进一行
	tips.1: 
	把中缀变成前缀 (==) 3 3
	把前缀变成中缀 3 `mod` 2

	tips.2:
	可以这样使用匹配 --arr@(x:xs) --p@(a, b)


类型
---------------------------------------	
基础类型：
	Bool, Int, String
复合类型:
	data MyBool = MFalse | MTrue
	--:t MTrue
	--MyBool::MyBool
	data MyMayBe a = MJust a | MNothing
	--:t MJust
	--MJust::a->MyMayBe
	(注， 这里myMayBe 叫做type constructor, 它的kind叫做*->*)

	data M1 a b = M1{f1::a, f2::b}
	--:t f1
	--f1 :: M1 a b -> a
	newtype(http://www.haskell.org/haskellwiki/Newtype)
	newtype 声明只有一个域(constructor or field)的data
	newtype M1 a b = M1{f1::a, f2::b}
	newtype State s a = State { runState :: s -> (s, a) }

	type 和 newtype区别	(http://www.haskell.org/haskellwiki/Type#Type_and_newtype)

	newtype apply a b = a->b

定义MyPair:
	data MyPair a b = MyPair {first::a, second::b}


可以定义MList:
	data Mlist a = Mconst a (Mlist a) | Mnil
	or)
	data Mlist a = Mcons (MyPair a (Mlist a) ) | Mnil

可以定义MTuple
	data MT a b c = MT {fa::a, fb::b, fc::c}
typeclass:
	常用的typeclass 有 Read, Show, Eq, Ord, Fuctor, Monad...

instance Show (MyBool) where
	show MTrue = "MTrue"
	show MFalse = "MFalse"

instance Eq (MyBool) where
	(==) MTrue MTrue = True
	(==) MFalse MFalse = True
	(==) _ _ = False

一些重要的typeclass
-----------------------------------
typeclass 可以继承
class  (Eq a) => Ord a  where
  (<), (<=), (>=), (>)  :: a -> a -> Bool
  max, min              :: a -> a -> a
不是所有接口都需要实现 	   
"Each type class defines a certain set of methods which need to be implemented; Eq requires == or /=, and Ord requires <= or compare."
(http://stackoverflow.com/questions/3065954/defining-your-own-ord-for-a-data-type-haskell)
理解 Ord
class (Eq a)=>(Ord a) where
	(<=)::a->a->Bool

instance Ord (MyBool) where
	(<=) MTrue MFalse = False
	(<=) _ _ = True
理解functor:
class Functor functor where
	fmap::(a->b)->(functor a)->(functor b)
理解applicative:
class (Functor app)=>Applicative app where:
	pure::a->(app a)
	<*> :: (app (a->b))->(app a)->(app b)
	($) ::(a->b)->(app a)->(app b)

IO
-----------------------------------
http://book.realworldhaskell.org/read/io.html

ghci 的一些使用小技巧
-----------------------------------
        :t :q :m :load
        ghci xxx.hs
	可以换行:
		:{--后起一行
		xxxxx
		:}

	因为每个 action 都是一个IO a 所以，可以使用do notation
	例：
		let f=xxx; let b=xxx; return f b
		a<-getLine 
-----------------------------------	
