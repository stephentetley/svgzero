namespace SvgZero

open System.Xml
open System.Xml.Linq

open SvgZero.Geometry
open SvgZero.GraphicProps
open SvgZero.SvgDoc
open SvgZero.PictureInternal

module OutputSvg = 


    /// Design note - the "trick" used by Wumpus to avoid printing style attributes unless they different
    /// from the parent graphic context is potentially not worthwhile ie. adds complexity for limited gain 
    /// if CSS properties or SVG use/defs can be used instead to minimize files size.
    ///
    /// This means SvgMonad might not need GraphicsState - potentially we might need to thread a CTM through
    /// output generation. 
    ///
    /// That said path styles (miter, cap-butt etc.) are so "rare" they might be better as local (override) 
    /// attributes. Path styles might be better modeled as a set of properties (only print ones in the set) 
    /// rather than a fixed record of properties. 


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


    let ellipseProps (attrs : ShapeProps) : SvgMonad<SvgAttribute list> = 
        unit []
        
    let primEllipse (attrs : ShapeProps) (obj : PrimEllipse) : SvgMonad<SvgElement> = 
        let arx = attrRx obj.HalfWidth
        let ary = attrRy obj.HalfHeight
        unit <| elemEllipse [ arx; ary ]
        