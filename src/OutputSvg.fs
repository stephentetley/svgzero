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
    
    let runSvg (ma : SvgMonad<'a>) : 'a = let (a,_) = apply1 ma () 0 in a

    let makeTspan rgb xelem = elemTspan [attrFill rgb] xelem

    let makeXY = function | P2(x,y) -> [ attrX x ; attrY y ]


    let shapeProps (props : ShapeProps) : SvgAttribute list = 
        let makeStroke : StrokeProps -> SvgAttribute list = 
            function | { StrokeColour =rgb; StrokeWidth = d} -> [attrStroke rgb; attrStrokeWidth d ]
        match props with
        | {ShapeFill = ofill; ShapeStroke = ostroke} -> 
            match ofill, ostroke with 
            | Some(fill), Some(stroke) -> attrFill fill :: makeStroke stroke                                                                                    
            | Some(fill), None -> [attrFill fill; attrStrokeNone ()]
            | None, Some(stroke) -> makeStroke stroke
            | None, None -> [attrStrokeNone ()]
            
                    
    let primEllipse1 (props : ShapeProps) (pt : Point2) (obj : PrimEllipse) : SvgElement = 
        let ps = shapeProps props
        let cs = [attrCx pt.GetX; attrCy pt.GetY]
        let rs = [attrRx obj.HalfWidth; attrRy obj.HalfHeight]
        elemEllipse <| ps @ rs @ cs

    /// Potentially we may have to change center point due to CTM...
    let primEllipse (props : ShapeProps) (pt : Point2) (obj : PrimEllipse) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primEllipse1 props pt obj 

    let primitive (prim : Primitive) : SvgMonad<SvgElement> = 
        match prim with
        | PEllipse(props,pt,obj) -> primEllipse props pt obj