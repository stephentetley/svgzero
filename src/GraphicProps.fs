namespace SvgZero


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
        | None | Underline | Overline | LineThrough | Blink
        
        member x.SvgValue = 
            match x with
            | None -> "none"
            | Underline -> "underline"
            | Overline -> "overline"
            | LineThrough -> "line-through"
            | Blink -> "blink"
    
    /// Potentially if we calculating bounding boxes for text we should always preserve space
    type Space = 
        | Default | Preserve
        
        member x.svgValue = 
            match x with
            | Default -> "default"
            | Preserve -> "preserve"