safetail1 xs = if null xs then [] else (tail xs)

safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 []     = []
safetail3 (_:xs) = xs

True || _ = True
_ || a = a


(&&) a b = if a then b else False
