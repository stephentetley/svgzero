namespace SvgZero

open System.Xml
open System.Xml.Linq


module SvgDoc = 
    let nssvg : XNamespace = XNamespace.Get("http://www.w3.org/2000/svg")

    let svgElem (name : string) (attrs : seq<XAttribute>) = new XElement (nssvg + name, attrs )  

    let svgElemB (name : string) (attrs : seq<XAttribute>) (elems : seq<XElement>) = 
        new XElement (XName.Get name, attrs, elems)  

    let svgElemNoAttrs (name : string) (elems : seq<XElement>) = 
        new XElement (XName.Get name, elems)  
    

    let elemSvg (body : seq<XElement>) : XElement = 
        let nsxlink : XNamespace = XNamespace.Get("http://www.w3.org/1999/xlink")
        let top = new XElement ( XName.Get ("svg", nssvg.NamespaceName ), 
                                 new XAttribute(XName.Get "version", "1.1"),
                                 new XAttribute(XNamespace.Xmlns + "xlink", nsxlink))
        top.Add body
        top
                               

    let attrFill (s : string) : XAttribute = new XAttribute(XName.Get "fill", s )

    let attrId (s : string) : XAttribute = new XAttribute(XName.Get "id", s )

    let attrR (d : double) : XAttribute = new XAttribute(XName.Get "r", d.ToString() )
    let attrRx (d : double) : XAttribute = new XAttribute(XName.Get "rx", d.ToString() )
    let attrRy (d : double) : XAttribute = new XAttribute(XName.Get "ry", d.ToString() )

    let attrCx (d : double) : XAttribute = new XAttribute(XName.Get "cx", d.ToString() )
    let attrCy (d : double) : XAttribute = new XAttribute(XName.Get "cy", d.ToString() )

    let elemCircle (attrs : seq<XAttribute>) : XElement = svgElem "circle" attrs
