namespace SvgZero


open SvgZero.Colour
open SvgZero.JoinList
open SvgZero.Geometry
open SvgZero.GraphicProps
open SvgZero.TransformInternal

module PictureInternal = 
    
    type SvgID = SvgID of string
    
    /// TODO - for simplicity, initially we should avoid adding a CTM to each primitive.
    /// We have already shown how to work with elements attributed with a CTM in Wumpus, we can crib
    /// this strategy later when we need to flesh out SvgZero's capabilities.
    ///
    /// Design note - supporting CSS styling (to minimize file size and allow independent change) 
    /// should probably be a higher priority than local (elementary) CTM transformations.
    
    
    
    /// For the time being don't bother with spaced text
    type LabelText = LabelText of string

        
    type PrimLabel = 
      { LabelBody : LabelText
        OptLabelId : Option<SvgID>
      }


      
    type PrimRectangle = 
      { Width : double
        Height : double
      }
        
    type PrimCircle = 
      { Radius : double
      }

    type PrimEllipse = 
      { HalfWidth : double
        HalfHeight : double
      }

    /// Paths - SVG supports both absolute and relative paths - should we support both?
    /// Probably - then a user can choose which makes sense for their application.
    
    type RelPathSegment = 
      | RelCurveTo of Vector2 * Vector2 * Vector2
      | RelLineTo of Vector2
      
    type PrimRelPath = 
      { RelPathStart : Point2 
        RelPathSegments : RelPathSegment list
      }
      
    type AbsPathSegment = 
      | AbsCurveTo of Point2 * Point2 * Point2
      | AbsLineTo of Point2
      
    type PrimAbsPath = 
      { AbsPathSegments : AbsPathSegment list
      }
      
    type PrimPath =
       | AbsolutePath of PrimAbsPath
       | RelativePath of PrimRelPath

    type PrimPolyline = { PolylinePoints : Point2 list }
    
    type PrimPolygon = { PolygonPoints : Point2 list }

    /// More to add (see Wumpus) ...
    type Primitive = 
      | PGroup of JoinList<Primitive>
      | PClip of PrimPath * Primitive
      | PLabel of LabelProps * Point2 * PrimLabel
      | PRectangle of RectProps * Point2 * PrimRectangle
      | PCircle of ShapeProps * Point2 * PrimCircle
      | PEllipse of ShapeProps * Point2 * PrimEllipse
      | PPolyline of StrokeProps * PrimPolyline
      | PPolygon of StrokeProps *  PrimPolygon

    type Picture = 
      | Leaf of JoinList<Primitive>
      | Picture of JoinList<Picture>

        
    /// Not sure if we need this, maybe it should just be a CTM when (/ if) we add
    /// that back
    type GraphicsState = unit
   