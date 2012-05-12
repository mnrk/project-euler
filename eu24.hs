import Data.List

eu24 = concat . map show . (!! 999999) . sort . lexPerms $ [0..9]

lexPerms r = do
  a <- r
  b <- r \\ [a]
  c <- r \\ (a:b:[])
  d <- r \\ (a:b:c:[])
  e <- r \\ (a:b:c:d:[])
  f <- r \\ (a:b:c:d:e:[])
  g <- r \\ (a:b:c:d:e:f:[])
  h <- r \\ (a:b:c:d:e:f:g:[])
  i <- r \\ (a:b:c:d:e:f:g:h:[])
  j <- r \\ (a:b:c:d:e:f:g:h:i:[])
  return (a:b:c:d:e:f:g:h:i:j:[])

--2783915460