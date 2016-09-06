namespace SvgZero

open System.Xml
open System.Xml.Linq

open SvgZero.JoinList
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
    
    let mapM (mf : 'a -> SvgMonad<'b>) (inp : 'a list) : SvgMonad<'b list> = 
        let rec work ys = 
            match ys with
            | [] -> svgoutput.Return []
            | z :: zs -> svgoutput { let! b = mf z
                                     let! bs = work zs
                                     return (b :: bs) }
        work inp

    let runSvg (ma : SvgMonad<'a>) : 'a = let (a,_) = apply1 ma () 0 in a

    let makeTspan rgb xelem = elemTspan [attrFill rgb] xelem

    let makeXY = function | P2(x,y) -> [ attrX x ; attrY y ]

    let labelProps (props : LabelProps) : SvgAttribute list = 
        let xs = []
        attrFill props.LabelColour :: xs

    let strokeProps (props : StrokeProps) : SvgAttribute list = 
        match props with
        | { StrokeColour =rgb; StrokeWidth = d} -> [attrStroke rgb; attrStrokeWidth d ]

    let shapeProps (props : ShapeProps) : SvgAttribute list = 
        match props with
        | {ShapeFill = ofill; ShapeStroke = ostroke} -> 
            match ofill, ostroke with 
            | Some(fill), Some(stroke) -> attrFill fill :: strokeProps stroke                                                                                    
            | Some(fill), None -> [attrFill fill; attrStrokeNone ()]
            | None, Some(stroke) -> strokeProps stroke
            | None, None -> [attrStrokeNone ()]
    
    let absPathSegment1 (obj : AbsPathSegment) : string = 
        match obj with
        | AbsCurveTo(p1,p2,p3) -> pathC p1 p2 p3
        | AbsLineTo(p1) -> pathL p1
        | AbsMoveTo(p1) -> pathM p1
    
    let relPathSegment1 (obj : RelPathSegment) : string = 
        match obj with
        | RelCurveTo(v1,v2,v3) -> pathRelC v1 v2 v3
        | RelLineTo(v1) -> pathRelL v1
        | RelMoveTo(v1) -> pathRelM v1

    
    let absPath (obj : PrimAbsPath) : string = 
        match obj with
        | {AbsPathStart = start; AbsPathSegments = segs} ->  
            let start1 = pathM start
            let parts = List.map absPathSegment1 segs
            String.concat " " <| start1 :: parts

    let relPath (obj : PrimRelPath) : string = 
        match obj with
        | {RelPathStart = start; RelPathSegments = segs} ->  
            let start1 = pathM start
            let parts = List.map relPathSegment1 segs
            String.concat " " <| start1 :: parts

    let primPath (obj : PrimPath) : SvgAttribute =  
        match obj with
        | AbsolutePath(path) -> attrD <| absPath path
        | RelativePath(path) -> attrD <| relPath path

    let primLabel1 (props : LabelProps) (pt : Point2) (obj : PrimLabel) : SvgElement =
        let attrs = labelProps props
        let cs = [attrX pt.GetX; attrY pt.GetY]
        let text = match obj.LabelBody with | LabelText(a) -> a
        elemText (attrs @ cs) text

    let primRect1 (props : RectProps) (pt : Point2) (obj : PrimRectangle) : SvgElement = 
        let ps = shapeProps props.RectProps
        let cs = [attrX pt.GetX; attrY pt.GetY]
        let wh = [attrWidth obj.Width; attrHeight obj.Height ]
        elemRect <| ps @ cs @ wh

    let primCircle1 (props : ShapeProps) (pt : Point2) (obj : PrimCircle) : SvgElement = 
        let ps = shapeProps props
        let cs = [attrCx pt.GetX; attrCy pt.GetY]
        let rs = [attrRx obj.Radius]
        elemCircle <| ps @ rs @ cs
                
    let primEllipse1 (props : ShapeProps) (pt : Point2) (obj : PrimEllipse) : SvgElement = 
        let ps = shapeProps props
        let cs = [attrCx pt.GetX; attrCy pt.GetY]
        let rs = [attrRx obj.HalfWidth; attrRy obj.HalfHeight]
        elemEllipse <| ps @ rs @ cs

    let primPolyline1 (props : StrokeProps) (points : PrimPolyline) : SvgElement = 
        let ps = strokeProps props
        let cs = match points with | {PolylinePoints = xs} -> [attrPoints xs]
        elemPolyline <| ps @ cs

    let primPolygon1 (props : ShapeProps) (points : PrimPolygon) : SvgElement = 
        let ps = shapeProps props
        let cs = match points with | {PolygonPoints = xs} -> [attrPoints xs]
        elemPolygon <| ps @ cs


    /// Potentially we may have to change center point due to CTM...
    let primLabel (props : LabelProps) (pt : Point2) (obj : PrimLabel) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primLabel1 props pt obj 

    let primRect (props : RectProps) (pt : Point2) (obj : PrimRectangle) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primRect1 props pt obj 

    let primCircle (props : ShapeProps) (pt : Point2) (obj : PrimCircle) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primCircle1 props pt obj 

    let primEllipse (props : ShapeProps) (pt : Point2) (obj : PrimEllipse) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primEllipse1 props pt obj 

    let primPolyline (props : StrokeProps) (ps : PrimPolyline) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primPolyline1 props ps

    let primPolygon (props : ShapeProps) (ps : PrimPolygon) : SvgMonad<SvgElement> = 
        svgoutput.Return <| primPolygon1 props ps

    let rec primitive (prim : Primitive) : SvgMonad<SvgElement> = 
        match prim with
        | PGroup(objs) -> group objs
        | PLabel(props,pt,obj) -> primLabel props pt obj
        | PRectangle(props,pt,obj) -> primRect props pt obj
        | PCircle(props,pt,obj) -> primCircle props pt obj
        | PEllipse(props,pt,obj) -> primEllipse props pt obj
        | PPolyline(props,obj) -> primPolyline props obj
        | PPolygon(props,obj) -> primPolygon props obj

    and group (objs : JoinList<Primitive>) : SvgMonad<SvgElement> = 
        svgoutput { let! body = mapM primitive (toList objs)
                    return (elemGNoAttrs body)
                  }

    let rec picture (obj : Picture) : SvgMonad<SvgElement> =
        match obj with
        | Leaf(prims) -> group prims
        | Picture(pics) ->  
            svgoutput { let! body = mapM picture (toList pics)
                        return (elemGNoAttrs body)
                      }

    let svgDraw (obj : Picture) : SvgDocument = 
        let body = runSvg (picture obj) in document body
