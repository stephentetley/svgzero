// open System;;
// Environment.CurrentDirectory <- @"E:\coding\fsharp\svgzero\svgzero";;
// #I @"E:\coding\fsharp\svgzero\svgzero";;
// #load @"Test.fsx";;
// open Test;;

#r "System.Xml"
#r "System.Xml.Linq"

#load "Colour.fs"
#load "JoinList.fs"
#load "Geometry.fs"
#load "GraphicProps.fs"
#load "SvgDoc.fs"
#load "TransformInternal.fs"
#load "PictureInternal.fs"
#load "OutputSvg.fs"

open System.Xml
open System.Xml.Linq

open SvgZero
open SvgZero.Colour
open SvgZero.JoinList
open SvgZero.Geometry
open SvgZero.GraphicProps
open SvgZero.SvgDoc
open SvgZero.PictureInternal
open SvgZero.OutputSvg

let test01 = elemSvg [ elemCircle <| seq [ attrCx 30.0; attrCy 80.0; attrR 20.0; attrFill (Named "blue") ] ]
let test02 = RGBAi(0,255,0,0).SvgValue;;

let doc = new XmlDocument ()
let nssvg : XNamespace = XNamespace.Get("http://www.w3.org/2000/svg")
let nsxlink : XNamespace = XNamespace.Get("http://www.w3.org/1999/xlink")

let linkname : XName = nsxlink.GetName("xlink")

let root = new XElement ( XName.Get ("svg", "http://www.w3.org/2000/svg")) 

let root2 = new XElement ( XName.Get ("svg", "http://www.w3.org/2000/svg"), 
                           new XAttribute(XName.Get "version", "1.1")
                         ) 

let root3 = new XElement ( XName.Get ("svg", nssvg.NamespaceName ), 
                           new XAttribute(XName.Get "version", "1.1"),
                           new XAttribute(XNamespace.Xmlns + "xlink", nsxlink)
                         ) 


let doc2 = 
   let d1 = new XDocument (new XDeclaration("1.0", "utf-8", "true")) 
   d1.Add root3
   d1
    

let testy01 = doc.ToString() 
let testy02 = doc2.ToString() 

let mdoc1 : SvgMonad<SvgElement> = 
    let fillRed = { ShapeFill= Some(Named("red")); ShapeStroke=None }
    let prim1 = PEllipse(fillRed, P2(0.0,0.0), { HalfWidth =5.0; HalfHeight=5.0 } )
    group <| fromList [prim1]
    


let testy03 = 
    let ma = mdoc1
    (fun d -> d.ToString()) <| runSvg ma

let testy04 = 
    let fillRed = { ShapeFill= Some(Named("red")); ShapeStroke=None }
    let prim1 = PEllipse(fillRed, P2(100.0, 50.0), { HalfWidth =5.0; HalfHeight=5.0 } )
    let doc1 = svgDraw <| Leaf (fromList [prim1])
    doc1.ToString()
    