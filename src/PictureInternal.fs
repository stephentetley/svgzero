namespace SvgZero


open SvgZero.Colour
open SvgZero.JoinList
open SvgZero.Geometry
open SvgZero.GraphicProps
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

    type PrimEllipse = 
      { HalfWidth : double
        HalfHeight : double
        EllipseCTM : PrimCTM
      }
        
    type PrimCircle = 
      { Radius : double
        CircleCTM : PrimCTM
      }
      
    type PrimRectangle = 
      { Width : double
        Height : double
        RectCTM : PrimCTM
      }
      
    /// Paths - SVG supports both absolute and relative paths - should we support both?
    /// Probably - then a user can choose which makes sense for their application.
    
    type RelPathSegment = 
      | RelCurveTo of Vector2 * Vector2 * Vector2
      | RelLineTo of Vector2
      
    type PrimRelPath = 
      { RelPathStart : Point2 
        RelPathSegments : RelPathSegment list
        RelPathCTM : PrimCTM
      }
      
    type AbsPathSegment = 
      | AbsCurveTo of Point2 * Point2 * Point2
      | AbsLineTo of Point2
      
    type PrimAbsPath = 
      { AbsPathSegments : AbsPathSegment list
        AbsPathCTM : PrimCTM
      }
      
    type PrimPath =
       | AbsolutePath of PrimAbsPath
       | RelativePath of PrimRelPath
      
    /// More to add (see Wumpus) ...
    type Primitive = 
      | PGroup of JoinList<Primitive>
      | PClip of PrimPath * Primitive
      | PLabel of LabelProps * PrimLabel
      | PEllipse of ShapeProps * PrimEllipse
      | PCircle of ShapeProps * PrimCircle
      | PRectangle of RectProps * PrimRectangle


    type GraphicsState = 
      { DrawColour : Colour
        Font : FontAttr
        Stroke : StrokeAttr
      }