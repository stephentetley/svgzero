// open System;;
// Environment.CurrentDirectory <- @"E:\coding\fsharp\svgzero\svgzero";;
// #I @"E:\coding\fsharp\svgzero\svgzero";;
// #load @"Test.fsx";;
// open Test;;

#r "System.Xml"
#r "System.Xml.Linq"

#load "SvgDoc.fs"

open SvgZero
open System.Xml
open System.Xml.Linq

let test01 = SvgDoc.elemSvg [ SvgDoc.elemCircle <| seq [ SvgDoc.attrCx 30.0; SvgDoc.attrCy 80.0; SvgDoc.attrR 20.0; SvgDoc.attrFill "blue" ] ]

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
