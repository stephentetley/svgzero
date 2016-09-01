namespace SvgZero

open SvgZero.Colour

module GraphicProps =

    /// Maybe need to pass in SvgValue function each time ('T -> string)
    /// let optSvgValue<'T> (f : 'T -> bool) (obj : 'T) : Option<string> = 
    ///     if f obj then None else Some <| obj.SvgValue
    
    type StrokeLinecap = 
        | Butt | Round | Square
        
        member x.SvgValue = 
            match x with
            | Butt -> "butt"
            | Round -> "round"
            | Square -> "square"
    
    type StrokeLinejoin = 
        | Miter | Round | Bevel
        
        member x.SvgValue = 
            match x with
            | Miter -> "miter"
            | Round -> "round"
            | Bevel -> "bevel"
        
    type FontStyle = 
        | Normal | Italic | Oblique
        
        member x.SvgValue = 
            match x with
            | Normal -> "normal"
            | Italic -> "italic"
            | Oblique -> "oblique"

    type FontVariant = 
        | Normal | SmallCaps
        
        member x.SvgValue = 
            match x with
            | Normal -> "normal"
            | SmallCaps -> "small-caps"
    
    type FontWeight = 
        | Normal | Bold | Bolder | Lighter | FW100 | FW200 | FW300 | FW400 | FW500 | FW600 | FW700 | FW800 | FW900
        
        member x.SvgValue = 
            match x with
            | Normal -> "normal"
            | Bold -> "bold"
            | Bolder -> "bolder"
            | Lighter -> "lighter"
            | FW100 -> "100"
            | FW200 -> "200"
            | FW300 -> "300"
            | FW400 -> "400"
            | FW500 -> "500"
            | FW600 -> "600"
            | FW700 -> "700"
            | FW800 -> "800"
            | FW900 -> "900"

        
    let private dasharrayValue (xs : (int * int) list) : string = failwith "help"
        
    /// Dash pattern either generates "stroke-dasharray" or nothing (solid stroke).
    type StrokeDasharray = 
        | Solid
        | Dasharray of (int * int) list
        
        member x.SvgValue = 
            match x with
            | Solid -> "none"
            | Dasharray (dasharray) -> dasharrayValue dasharray
    
    type TextDecoration = 
        | NoDecoration | Underline | Overline | LineThrough | Blink
        
        member x.SvgValue = 
            match x with
            | NoDecoration -> "none"
            | Underline -> "underline"
            | Overline -> "overline"
            | LineThrough -> "line-through"
            | Blink -> "blink"
    
    /// Potentially if we calculating bounding boxes for text we should always preserve space
    type Space = 
        | Default | Preserve
        
        member x.SvgValue = 
            match x with
            | Default -> "default"
            | Preserve -> "preserve"


    /// Optional stroke attributes for drawing paths.
    /// Note stroke-width and stroke-colour are considered a mandatory attributes.
    type StrokeAttr = 
      | MiterLimit of double
      | LineCap of StrokeLinecap
      | LineJoin of StrokeLinejoin
      | DashPattern of StrokeDasharray



    /// Difference to Wumpus (Wumpus probably got it wrong):
    /// Closing a path now seems more related to its shape (points) than its style (stroke attributes /
    /// colour).

    type PathProps = 
      { PathColour : Colour
        LineAttrs : StrokeAttr list
      }
   
        
    type FontFace = { FontName : string }
    

    type FontAttr =  
      | FontSize of int 
      | FontFace of FontFace
      | FontStyle of FontStyle
      | FontVariant of FontVariant
      | FontWeight of FontWeight
        
    type LabelProps = 
      { LabelColour : Colour
        FontAttrs : FontAttr list
      }
    
    /// Note - SVG has builtins for:
    /// Rect Circle Ellipse Line Polyline Polygon
    /// Having (Rect, Circle, Ellipse) in both input and output is a good optimization.
    
    /// We might as well model Polygon and Polyline as we want a graphic model close
    /// to SVG. If we model Polyline we can recover Line as one segment Polylines
    
    /// Note - RoundingX and RoundingY should probably be cases of a sum type rather than fields 
    /// of a record. A list of properties (to always render) better models optional SVG attributes 
    /// rather than mandatory record fields that we might chose not to print if they are the default 
    /// value. This allows local overrides if we use a style sheet.
    ///
    /// This also applies to stroke attributes of paths and font attributes of labels.

    type StrokeProps =
      { StrokeColour : Colour
        StrokeWidth : double
      }      
    
    /// Circle, Ellipse, Polygon
    /// Shape can be filled (no border), stroked (just border) or filled-stroked (background fill and border)
    /// If it has no Fill or Stroke props it should ... not be rendered? (or inherit defaults?)
    type ShapeProps =
      { ShapeFill : Option<Colour>
        ShapeStroke : Option<StrokeProps>
      }
    
    /// Rect a special shape - can have rounded corners
    type RectAttr = 
      | RectRounding of double * double
      
    type RectProps = 
      { RectAttrs : RectAttr list
        RectProps : ShapeProps
      }

        
        