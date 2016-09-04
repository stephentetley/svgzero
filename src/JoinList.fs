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



    let one (x : 'a) : JoinList<'a> = Item(x)
    
    let cons (x : 'a) (xs : JoinList<'a>) : JoinList<'a> = Join(Item(x), xs)
    
    let snoc (xs : JoinList<'a>) (x : 'a) : JoinList<'a> = Join(xs, Item(x))

    let toList (xs : JoinList<'a>) : 'a list = joinfoldr (fun ys y -> y :: ys) [] xs

    let fromList (xs : 'a list) : JoinList<'a> = List.foldBack cons xs Empty
        
    /// Implementation detail - don't add Emptys
    let append (xs : JoinList<'a>) (ys : JoinList<'a>) = 
        match xs,ys with 
        | Empty, zs -> zs
        | zs, Empty -> zs
        | _, _ -> Join (xs,ys)

    /// Safe-head function
    let tryHead (xs : JoinList<'a>) : 'a option = 
        let rec step ys = 
            match ys with
            | Empty -> None
            | Item(a) -> Some(a)
            | Join(a,b) -> 
                match step a with
                | None -> step b
                | Some(a) -> Some(a)
        step xs
        
    type ViewLeft<'a> = 
        | EmptyLeftView
        | LeftList of 'a * JoinList<'a>
    
    let viewLeft (xs : JoinList<'a>) : ViewLeft<'a> = 
        let rec step ys = 
            match ys with
            | Empty -> EmptyLeftView
            | Item(a) -> LeftList(a, Empty)
            | Join(left,right) -> 
                match step left with
                | EmptyLeftView -> step right
                | LeftList(a,b) -> LeftList(a, append b right)
        step xs
