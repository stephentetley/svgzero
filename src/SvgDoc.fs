﻿namespace SvgZero

open System.Xml
open System.Xml.Linq

open SvgZero.Colour
open SvgZero.Geometry
open SvgZero.GraphicProps

module SvgDoc = 
    
    
    /// Aliases so we don't need to reference System.Xml.Linq namespace everywhere
    type SvgElement = XElement
    type SvgAttribute = XAttribute
    type SvgText = XText
    type SvgDocument = XDocument

    let nssvg : XNamespace = XNamespace.Get("http://www.w3.org/2000/svg")

    let svgElem (name : string) (attrs : seq<SvgAttribute>) = new SvgElement (nssvg + name, attrs )  

    let svgElemBs (name : string) (attrs : seq<SvgAttribute>) (elems : seq<SvgElement>) = 
        new SvgElement (nssvg + name, attrs, elems)  
        
    let svgElemB1 (name : string) (attrs : seq<SvgAttribute>) (elem : SvgElement) = 
        new SvgElement (nssvg + name, attrs, elem) 

    let svgElemNoAttrs (name : string) (elems : seq<SvgElement>) = 
        new SvgElement (nssvg + name, elems)  
    
    let svgAttr (name : string) (value : string) : SvgAttribute = new SvgAttribute(XName.Get name, value )

    let svgAttrs (name : string) (values : seq<string>) : SvgAttribute = svgAttr name (String.concat " " values)

    let elemSvg (body : seq<SvgElement>) : SvgElement = 
        let nsxlink : XNamespace = XNamespace.Get("http://www.w3.org/1999/xlink")
        let top = new SvgElement ( XName.Get ("svg", nssvg.NamespaceName ), 
                                 new SvgAttribute(XName.Get "version", "1.1"),
                                 new SvgAttribute(XNamespace.Xmlns + "xlink", nsxlink))
        top.Add body
        top
                               

    let elemG (attrs : seq<SvgAttribute>) (body : seq<SvgElement>) = svgElemBs "g" attrs body
    
    let elemG1 (attrs : seq<SvgAttribute>) (body : SvgElement) = svgElemB1 "g" attrs body
    
    let elemGNoAttrs (body : seq<SvgElement>) = svgElemNoAttrs "g" body
    
    let elemClipPath (attrs : seq<SvgAttribute>) (body : seq<SvgElement>) = svgElemBs "clipPath" attrs body
    
    let elemPath (attrs : seq<SvgAttribute>) (path : string) = 
        let attrs2 = Seq.append attrs (Seq.singleton <| svgAttr "d" path)
        svgElem "path" attrs2
    
    let elemPathNoAttrs (path : string) = svgElem "path" [svgAttr "d" path]
    
    let elemText (attrs : seq<SvgAttribute>) (body : string) = new SvgElement (XName.Get "text", attrs, new XText(body))

    let elemTspan (attrs : seq<SvgAttribute>) (body : SvgElement) : SvgElement = svgElemB1 "tspan" attrs body
    
    let elemRect (attrs : seq<SvgAttribute>) : SvgElement = svgElem "rect" attrs

    let elemCircle (attrs : seq<SvgAttribute>) : SvgElement = svgElem "circle" attrs
    
    let elemEllipse (attrs : seq<SvgAttribute>) : SvgElement = svgElem "ellipse" attrs
    
    let elemPolyline (attrs : seq<SvgAttribute>) : SvgElement = svgElem "polyline" attrs

    let elemPolygon (attrs : seq<SvgAttribute>) : SvgElement = svgElem "polygon" attrs

    let attrId (s : string) : SvgAttribute = new SvgAttribute(XName.Get "id", s )
    
    let attrX (d : double) = svgAttr "x" (d.ToString())
    
    /// Seq version of attrX
    let attrXs (ds : seq<double>) = svgAttrs "x" <| Seq.map (fun d -> (d.ToString())) ds
    
    let attrY (d : double) = svgAttr "y" (d.ToString())
    
    /// Seq version of attrY
    let attrYs (ds : seq<double>) = svgAttrs "y" <| Seq.map (fun d -> (d.ToString())) ds
    
    let attrWidth (d : double) : SvgAttribute = svgAttr "width" (d.ToString())
    let attrHeight (d : double) : SvgAttribute = svgAttr "height" (d.ToString())

    let attrR (d : double) : SvgAttribute = svgAttr "r" (d.ToString())
    let attrRx (d : double) : SvgAttribute = svgAttr "rx" (d.ToString())
    let attrRy (d : double) : SvgAttribute = svgAttr "ry" (d.ToString())

    let attrCx (d : double) : SvgAttribute = svgAttr "cx" (d.ToString())
    let attrCy (d : double) : SvgAttribute = svgAttr "cy" (d.ToString())

    /// path data already rendered to a string...
    let attrD (ss : string) : SvgAttribute = svgAttr "d" ss
    
    /// Path Segments, encoded as string values.
    
    /// M ... ...
    /// c.f. PostScript's @moveto@.
    let pathM (p1 : Point2) : string = sprintf "M %f %f" p1.GetX p1.GetY
    
    /// L ... ...
    /// c.f. PostScript's @lineto@.
    let pathL (p1 : Point2) : string = sprintf "L %f %f" p1.GetX p1.GetY
    
    /// C ... ... ... ... ... ... 
    /// c.f. PostScript's @curveto@.
    let pathC (p1 : Point2) (p2 : Point2) (p3 : Point2) : string = 
        sprintf "C %f %f %f %f %f %f" p1.GetX p1.GetY p2.GetX p2.GetY p3.GetX p3.GetY
    
    /// m ... ...
    /// c.f. PostScript's @moveto@.
    let pathRelM (v1 : Vector2) : string = sprintf "m %f %f" v1.GetX v1.GetY
    
    /// l ... ...
    /// c.f. PostScript's @lineto@.
    let pathRelL (v1 : Vector2) : string = sprintf "l %f %f" v1.GetX v1.GetY
    
    /// c ... ... ... ... ... ... 
    /// c.f. PostScript's @curveto@.
    let pathRelC (v1 : Vector2) (v2 : Vector2) (v3 : Vector2) : string = 
        sprintf "C %f %f %f %f %f %f" v1.GetX v1.GetY v2.GetX v2.GetY v3.GetX v3.GetY


    let attrPoints (ps : Point2 list) : SvgAttribute = 
        let point1 = function | P2(x,y) -> sprintf "%f,%f" x y
        let body = String.concat " " <| List.map  point1 ps
        svgAttr "points" body
    
    /// font-family="..."
    let attrFontFamily (name : string) = svgAttr "font-family" name
    
    /// font-size="..."
    let attrFontSize (i : int) = svgAttr "font-size" (i.ToString())
    
    /// font-style="..."
    let attrFontStyle (x : FontStyle) = svgAttr "font-style" x.SvgValue
    
    /// font-weight="..."
    let attrFontWeight (x : FontWeight) = svgAttr "font-weight" x.SvgValue
    
    /// font-variant="..."
    let attrFontVariant (x : FontVariant) = svgAttr "font-variant" x.SvgValue
    
    
    
    /// fill="rgba(...,...,...,...)"
    /// fill="black"
    let attrFill (x : Colour) = svgAttr "fill" x.SvgValue
    
    /// fill="none"
    /// For stroke - no fill
    let attrFillNone () = svgAttr "fill" "none"
    
    /// stroke="rgba(...,...,...,...)"
    /// stroke="black"
    let attrStroke (x : Colour) = svgAttr "stroke" x.SvgValue

    /// stroke="none"
    /// For fill - no stroke
    let attrStrokeNone () = svgAttr "stroke" "none"
    
    /// stroke-width="..."
    let attrStrokeWidth (d : double) = svgAttr "stroke-width" (d.ToString())
    
    /// stroke-miterlimit="..."
    let attrStrokeMiterlimit (d : double) = svgAttr "stroke-miterlimit" (d.ToString()) 
    
    /// stroke-linecap="..." 
    let attrStrokeLinecap (x : StrokeLinecap) = svgAttr "stroke-linecap" x.SvgValue
    
    /// stroke-linejoin="..."
    let attrStrokeLinejoin (x : StrokeLinejoin) = svgAttr "stroke-linejoin" x.SvgValue
    
    /// clip_path="url(#...)"
    let attrClipPath (s : string) = svgAttr "clip-path" (sprintf "url(#%s)" s)
    
    /// SVG allows multiple transforms spearated by space
    let attrTransform (s : string) = svgAttr "transform" s
    
    let valMatrix (m : Matrix3x3) : string = 
        match deconsMatrix m with
            | (a,b,c,d,e,f) -> sprintf "matrix(%f,%f,%f,%f,%f,%f)" a b c d e f
    
    let valTranslate (v : Vector2) : string = sprintf "translate(%f,%f)" v.GetX v.GetY
    
    let valRotate (r : Radians) : string = sprintf "rotate(%f)" (rad2Deg r)
    
    let valScale (x : double) (y : double) = sprintf "scale(%f,%f)" x y
    
    let document (content : SvgElement) : SvgDocument = 
        let d1 = new XDocument (new XDeclaration("1.0", "utf-8", "true")) 
        let nsxlink : XNamespace = XNamespace.Get("http://www.w3.org/1999/xlink")
        let root = new XElement ( XName.Get ("svg", nssvg.NamespaceName ), 
                                  new XAttribute(XName.Get "version", "1.1"),
                                  new XAttribute(XNamespace.Xmlns + "xlink", nsxlink)
                                ) 
        root.Add content
        d1.Add root
        d1