module Lambda where

import Debug.Trace

data TPrimary = TNil | TagBool | TagInt | TagString deriving (Show)

data TCompound = 
	TagAnd [TType] |
	TagOr [TType] |
	TagArrow TType TType |
	TagLambda TExp |
	TagApply TEnv TExp
	deriving (Show)

data TType = 
	TagPrimary TPrimary |
	TagCompound TCompound |
	TagArg Int |
	TagValue Int
	deriving (Show)

data TExp =
	TagExp TEnv TType |
	TagAdd TType TType
	deriving (Show)
	
type TEnv = [(Int, TType)]

addTable t1 t2 = --t2 override t1
	let 
		ret1 = 
			foldl
				(\acc item@(k, v)->
					let r = lookup k t2
					in
						case r of
							Just v1-> (k, v1):acc
							Nothing-> (k, v):acc)
				[] t1
	in
			foldl
				(\acc item@(k, v)->
					let r = lookup k acc
					in 
						case r of
							Nothing->item:acc
							Just n->acc)
				ret1 t2


eval::TEnv->TType->TType
eval tenv tin =
	case tin of
			TagArg arg -> 
				let val = lookup arg tenv
				in
					case val of
						Just ret->ret
						_ -> tin
			TagCompound comp ->
				case comp of
					TagAnd tarray->
						TagCompound $ TagAnd $ fmap (eval tenv) tarray
					TagOr tarray->
						TagCompound $ TagOr $ fmap (eval tenv) tarray
					TagLambda texp->
						case texp of
							TagExp env1 t1->
								TagCompound $ TagLambda $ TagExp (addTable tenv env1) t1
							_ -> tin
					TagApply env1 exp1 ->
						case exp1 of
							TagExp env2 t1->eval (addTable env1 env2) t1
							TagAdd t1 t2->
								let
									env3 = addTable tenv env1
									arg1 = eval env3 t1
									arg2 = eval env3 t2
								in
									case arg1 of
										TagValue i1->
											case arg2 of
												TagValue i2-> TagValue (i1+i2)
												_->error "arg2 is not int"
										_->error "arg1 is not int"
			_->tin
			
										
								
					
t1 = eval [] (TagCompound $ TagApply [(1, TagValue 1), (2, TagValue 2)] (TagAdd (TagArg 1) (TagArg 2)) )
t2 = TagCompound $ TagLambda $ TagAdd (TagArg 1) (TagValue 3)
t3 = let TagCompound (TagLambda exp) = t2 in eval [] $ (TagCompound $ TagApply [(1, TagValue 1)] exp)




