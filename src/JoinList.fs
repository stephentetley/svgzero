namespace SvgZero


module JoinList = 
    
    /// Arguably Wumpus was too fussy about avoiding non-empty lists
    /// We'll see if we can live with them for the present...

    type JoinList<'a> = 
        | Empty 
        | Item of 'a
        | Join of JoinList<'a> * JoinList<'a>
        
    let rec map (f : 'a -> 'b) (c : JoinList<'a>) : JoinList<'b> = 
        match c with
        | Empty -> Empty
        | Item(a) -> Item(f a)
        | Join(a,b) -> Join(map f  a, map f b)


    let joinfoldr (f : 'b -> 'a -> 'b) (start : 'b) (xs : JoinList<'a>) : 'b = 
        let rec go ac e = 
            match e with
            | Empty -> ac
            | Item(a) -> f ac a
            | Join(t,u) -> let ac1 = go ac u in go ac1 t
        go start xs

    let toList (xs : JoinList<'a>) : 'a list = joinfoldr (fun ys y -> y :: ys) [] xs
