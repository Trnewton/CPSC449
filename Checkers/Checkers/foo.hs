foo xs = [(x,y)|
            x <- xs, y <- xs,
            x + y <= 4]