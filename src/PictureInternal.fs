namespace SvgZero

open SvgZero.TransformInternal

module PictureInternal = 
    
    type SvgID = SvgID of string
    
    /// A char paired with its displacement from the previous char.
    /// Used for display text.
    type SpacedChar = SpacedChar of double * char
    
    type LabelText = 
        | LabelBody of string
        | SpacedH of SpacedChar list
        | SpacedV of SpacedChar list
        
    type PrimLabel = 
        { LabelBody : LabelText; OptLabelId : Option<SvgID>; LabelCTM : PrimCTM }