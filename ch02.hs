-- 3
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 4
last' = head . reverse

-- 5
init' = reverse . tail . reverse

init'' xs = take (length xs - 1) xs