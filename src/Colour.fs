namespace SvgZero

module Colour = 

    /// To be compatible with SVG we should support alpha value as well as RGB.
    
    type RGBAi = 
        | RGBAi of int * int * int * int
        member x.SvgValue = match x with | RGBAi(r,g,b,a) -> sprintf "rgba(%i,%i,%i,%i)" r g b a
    
    /// Ideally if using named colours we should be able to see named colours in the output.
    type Colour = 
        | RGBA of RGBAi
        | Named of string
        
        member x.SvgValue = 
            match x with
            | RGBA rgba -> rgba.SvgValue
            | Named s -> s