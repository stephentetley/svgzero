namespace SvgZero

open System.Xml
open System.Xml.Linq

open SvgZero.Geometry
open SvgZero.GraphicProps
open SvgZero.SvgDoc
open SvgZero.PictureInternal

module SvgOutput = 

    /// Design note - the "trick" used by Wumpus to avoid printing style attributes unless they different
    /// from the parent graphic context is potentially not worthwhile (ie. adds complexity) if CSS properties 
    /// or SVG use/defs can be used instead to minimize files size.

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
        

    let makeTspan rgb xelem = elemTspan [attrFill rgb] xelem

    let makeXY = function | P2(x,y) -> [ attrX x ; attrY y ]

    /// Design note - kerning (stretched text) is probably a distraction at the moment...

    /// This is for horizontal kerning text, the output is of the 
    /// form:
    ///  
    /// > x="0 10 25 35" y="0"
    ///
    /// makeXsY :: DPoint2 -> [KerningChar] -> Doc
    let makeXsY (pt : Point2) (ks : SpacedChar list) : XAttribute list = 
        let rec step ax ls = 
            match ls with 
            | (d,_) :: ds -> let a = ax+d in a :: step a ds 
            | [] -> []
        [ attrXs (step pt.GetX ks); attrY pt.GetY ]

    let ellipseProps (attrs : ShapeProps) : SvgMonad<SVGAttribute list> = 
        unit []
        
    let primEllipse (attrs : ShapeProps) (obj : PrimEllipse) : SvgMonad<SVGElement> = 
        let arx = attrRx obj.HalfWidth
        let ary = attrRy obj.HalfHeight
        unit <| elemEllipse [ arx; ary ]
        