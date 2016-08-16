namespace SvgZero

open System.Xml
open System.Xml.Linq

open SvgZero.Colour
open SvgZero.Geometry
open SvgZero.GraphicProps

module SvgDoc = 
    


    let nssvg : XNamespace = XNamespace.Get("http://www.w3.org/2000/svg")

    let svgElem (name : string) (attrs : seq<XAttribute>) = new XElement (nssvg + name, attrs )  

    let svgElemBs (name : string) (attrs : seq<XAttribute>) (elems : seq<XElement>) = 
        new XElement (XName.Get name, attrs, elems)  
        
    let svgElemB1 (name : string) (attrs : seq<XAttribute>) (elem : XElement) = 
        new XElement (XName.Get name, attrs, elem) 

    let svgElemNoAttrs (name : string) (elems : seq<XElement>) = 
        new XElement (XName.Get name, elems)  
    
    let svgAttr (name : string) (value : string) : XAttribute = new XAttribute(XName.Get name, value )

    let elemSvg (body : seq<XElement>) : XElement = 
        let nsxlink : XNamespace = XNamespace.Get("http://www.w3.org/1999/xlink")
        let top = new XElement ( XName.Get ("svg", nssvg.NamespaceName ), 
                                 new XAttribute(XName.Get "version", "1.1"),
                                 new XAttribute(XNamespace.Xmlns + "xlink", nsxlink))
        top.Add body
        top
                               

    let elemG (attrs : seq<XAttribute>) (body : seq<XElement>) = svgElemBs "g" attrs body
    
    let elemG1 (attrs : seq<XAttribute>) (body : XElement) = svgElemB1 "g" attrs body
    
    let elemGNoAttrs (body : seq<XElement>) = svgElemNoAttrs "g" body
    
    let elemClipPath (attrs : seq<XAttribute>) (body : seq<XElement>) = svgElemBs "clipPath" attrs body
    
    let elemPath (attrs : seq<XAttribute>) (path : string) = 
        let attrs2 = Seq.append attrs (Seq.singleton <| svgAttr "d" path)
        svgElem "path" attrs2
    
    let elemPathNoAttrs (path : string) = svgElem "path" [svgAttr "d" path]
    
    let elemText (attrs : seq<XAttribute>) (body : seq<XElement>) = svgElemBs "text" attrs body

    let elemTspan (attrs : seq<XAttribute>) (body : XElement) : XElement = svgElemB1 "tspan" attrs body
    
    let elemCircle (attrs : seq<XAttribute>) : XElement = svgElem "circle" attrs
    
    let elemEllipse (attrs : seq<XAttribute>) : XElement = svgElem "ellipse" attrs
    
    let attrId (s : string) : XAttribute = new XAttribute(XName.Get "id", s )
    
    let attrX (d : double) = svgAttr "x" (d.ToString())
    
    /// Seq version of attrX
    let attrXs (ds : seq<double>) = 
        let ss = Seq.map (fun d -> (d.ToString())) ds in svgAttr "x" (String.concat " " ss)
    
    let attrY (d : double) = svgAttr "y" (d.ToString())
    
    /// Seq version of attrY
    let attrYs (ds : seq<double>) = 
        let ss = Seq.map (fun d -> (d.ToString())) ds in svgAttr "y" (String.concat " " ss)
    
    let attrR (d : double) : XAttribute = new XAttribute(XName.Get "r", d.ToString() )
    let attrRx (d : double) : XAttribute = new XAttribute(XName.Get "rx", d.ToString() )
    let attrRy (d : double) : XAttribute = new XAttribute(XName.Get "ry", d.ToString() )

    let attrCx (d : double) : XAttribute = new XAttribute(XName.Get "cx", d.ToString() )
    let attrCy (d : double) : XAttribute = new XAttribute(XName.Get "cy", d.ToString() )

    
    
    /// Path Segments, encoded as string values.
    
    /// M ... ...
    /// c.f. PostScript's @moveto@.
    let pathM (p1 : Point2) : string = sprintf "M %f %f" p1.GetX p1.GetY
    
    /// L ... ...
    /// c.f. PostScript's @lineto@.
    let pathL (p1 :Point2) : string = sprintf "L %f %f" p1.GetX p1.GetY
    
    /// C ... ... ... ... ... ... 
    /// c.f. PostScript's @curveto@.
    let pathC (p1 : Point2) (p2 : Point2) (p3 : Point2) : string = 
        sprintf "C %f %f %f %f %f %f" p1.GetX p1.GetY p2.GetX p2.GetY p3.GetX p3.GetY
    
    
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
    
    let valRotate (r : Radian) : string = sprintf "rotate(%f)" (rad2Deg r)
    
    let valScale (x : double) (y : double) = sprintf "scale(%f,%f)" x y
    
    
    