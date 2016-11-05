> data List a = Empty
>             | Cons a (List a)

> toList :: [a] -> List a
> toList [] = Empty
> toList (a:as)= Cons a (toList as)


> fromList :: List a -> [a]
> fromList Empty = []
> fromList (Cons a as) = a : (fromList as)


> data Fix f = In (f (Fix f))
>
> data List' a k = Empty'
>                | Cons' a k

> toList' :: [a] -> Fix (List' a)
> toList' = foldr f k where
>   k :: Fix (List' a)
>   k = In (Empty')

>   f :: a -> Fix (List' a) -> Fix (List' a)
>   f x ys = In (Cons' x ys)

> fromList' :: Fix (List' a) -> [a]
> fromList' (In (Empty'))     = []
> fromList' (In (Cons' x xs)) = x : fromList' xs

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop
