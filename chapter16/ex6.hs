data Tree = Leaf Int | Node Tree Tree

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

countNodes :: Tree -> Int
countNodes (Leaf _) = 0
countNodes (Node l r) = countNodes l + 1 + countNodes r

-- countNodes t + 1 = countLeaves t

-- Base case
-- countNodes (Leaf _) + 1
-- = { applying countNodes }
-- 0 + 1
-- = { applying + }
-- 1
-- = { unapplying countLeaves }
-- countLeaves (Leaf _)

-- Inductive case
-- countNodes (Node l r) + 1
-- = { applying countNodes }
-- countNodes l + 1 + countNodes r + 1
-- = { induction hypothesis on l }
-- countLeaves l + countNodes r + 1
-- = { induction hypothesis on r }
-- countLeaves l + countLeaves r
-- = { unapplying countLeaves }
-- countLeaves (Node l r)