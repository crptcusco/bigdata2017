main :: IO()
mdc 0 b = b
mdc a 0 = a 
mdc a b = mdc b (a `rem` b) 
main = do
 print (mdc 5 30)
