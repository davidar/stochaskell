~p :: P (R,R)~
p = do a <- normal 0 1
       b <- normal 0 1
       return (2 * a, a + b)

let [x,y] = symbols "xy"
 in solveP p (x,y)