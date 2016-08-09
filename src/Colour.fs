namespace SvgZero

module Colour = 

    /// To be compatible with SVG we should support alpha value as well as RGB.
    
    type RGBAi = 
        { Red : int; Green : int; Blue : int; Alpha : int }
        member x.SvgValue = sprintf "rgba(%i,%i,%i,%i)" x.Red x.Green x.Blue x.Alpha
    
    /// Ideally if using named colours we should be able to see named colours in the output.
    type Colour = 
        | RGBAi of RGBAi
        | Named of string
        
        member x.SvgValue = match x with
            | RGBAi rgba -> rgba.SvgValue
            | Named s -> s