namespace SvgZero

open SvgZero.PictureInternal

module SvgOutput = 

    type ClipCount = int

    type SvgMonad<'a> = SvgMonad of (GraphicsState -> ClipCount -> ('a * ClipCount))

    let apply1 (ma : SvgMonad<'a>) (gs : GraphicsState) (s : ClipCount) : ('a * ClipCount) = 
        let (SvgMonad f) = ma in f gs s

    let unit (x : 'a) : SvgMonad<'a> = SvgMonad (fun _ s -> (x,s))

    let bind (ma : SvgMonad<'a>) (f : 'a -> SvgMonad<'b>) : SvgMonad<'b> =
        SvgMonad (fun gs s -> let (a,s1) =  apply1 ma gs s in apply1 (f a) gs s1)

    type SvgMonadBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f

    let svgoutput = new SvgMonadBuilder()
        

