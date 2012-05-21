{- |
 - Value comparison type.
 -
 - Match can been used to construct rich comparisons that can themselves be
 - compared.
 -
 - Example:
 -
 -      -- While you can't do this:
 -      cmpA = (== 5)
 -      cmpB = (== 5)
 -      cmpA == cmpB   -- This will fail!  No instance for Eq (a -> a)
 -
 -      -- You can do this:
 -      cmpA = Exactly 5
 -      cmpB = Exactly 5
 -      cmpA == cmpB
 -
 -      -- Then go on to use the comparison:
 -      5 ~= cmpA
 -}

module Data.Match (Match(..), (~=), between) where

data Match a = Everything      -- ^ all values
             | Exactly a       -- ^ exactly a
             | LessThan a      -- ^ all values less than
             | GreaterThan a   -- ^ all values greater than
             | LessThanEq a    -- ^ all values less than or equal
             | GreaterThanEq a -- ^ all values greater than or equal
             | All [Match a]   -- ^ all submatches must succeed
             | Any [Match a]   -- ^ any successful match
             | Not (Match a)   -- ^ only matches if embedded match fails
             deriving (Eq, Show)

-- | Is value within defined constraints of Match value?
(~=) :: (Ord a, Eq a) => a -> Match a -> Bool
_ ~=  Everything       = True
a ~= (Exactly b)       = a == b
a ~= (LessThan b)      = a < b
a ~= (GreaterThan b)   = a > b
a ~= (LessThanEq b)    = a <= b
a ~= (GreaterThanEq b) = a >= b
a ~= (All bs)          = all (a ~=) bs
a ~= (Any bs)          = any (a ~=) bs
a ~= (Not b)           = not $ a ~= b

-- | Construct between (inclusive) comparison
between :: Ord a => a -> a -> Match a
between a b = All [GreaterThanEq a, LessThanEq b]

