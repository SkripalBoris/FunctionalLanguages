data RevList a = Snoc (RevList a) a | RNil

instance (Show a)=>Show (RevList a) where
    show RNil             = "[]"
    show (Snoc init last) = showRevList init ++ show last ++ "]" where showRevList RNil             = "["
                                                                       showRevList (Snoc init last) = showRevList init ++ show last ++ ", "

instance (Eq a)=>Eq (RevList a) where
    RNil == RNil                         = True
    Snoc inita lasta == Snoc initb lastb = inita == initb && lasta == lastb

instance  (Eq a, Ord a) => Ord (RevList a)  where 
    RNil <= RNil                         = True
    Snoc inita lasta <= Snoc initb lastb = inita <= initb && lasta <= lastb

instance Monoid (RevList a) where
    mempty                                        = RNil
    mappend RNil a                                = a
    mappend a RNil                                = a
    mappend (Snoc inita lasta) (Snoc initb lastb) = mappendRevList (Snoc inita lasta)(Snoc initb lastb) where mappendRevList a RNil             = a
                                                                                                              mappendRevList a (Snoc init last) = Snoc (mappendRevList a init) last

instance Functor RevList where
    fmap f RNil             = RNil
    fmap f (Snoc init last) = Snoc (fmap f init) (f last)

instance Applicative RevList where
    pure a                                    = Snoc RNil a
    (Snoc inita lasta) <*> (Snoc initb lastb) = Snoc (inita <*> initb) (lasta lastb)
    _ <*> _                                   = RNil

instance Monad RevList where
    return a             = Snoc RNil a
    RNil >>= _           = RNil
    Snoc init last >>= f = summ (init >>= f) (f last) where summ RNil a               = a
                                                            summ a RNil               = a
                                                            summ a (Snoc initb lastb) = Snoc (summ a initb) lastb

list=Snoc(Snoc (Snoc RNil 1)2)3
list2=Snoc(Snoc (Snoc RNil 4)5)6

main = do
    print(show(list==list2))
    print(show(list/=list))
    print(show(list==list))
    print(show(list<=list2))
    print(show(list>=list2))
    print(show( list `mappend` list2))
    print(show(fmap (+42) list))
    print(show( list >>= \x -> return (x + 2)))