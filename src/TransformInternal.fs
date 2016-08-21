namespace SvgZero

open SvgZero.Geometry

module TransformInternal = 


    /// Reify affine transformations as syntax.
    type AffineOperation = 
        | ApplyMatrix of Matrix3x3
        | RotateBy of Radians
        | RotateAbout of Radians * Point2
        | ScaleBy of double * double
        | TranslateBy of double * double
        
    let evaluateAffineOperation (op : AffineOperation) : Matrix3x3 = 
        match op with
        | ApplyMatrix(m) -> m
        | RotateBy(theta) -> rotationMatrix theta
        | RotateAbout(theta,pt) -> originatedRotationMatrix theta pt
        | ScaleBy(sx,sy) -> scalingMatrix sx sy 
        | TranslateBy(dx,dy) -> translationMatrix dx dy
        
        
    /// Do we need to implement PrimCTM from Wumpus? - Looks like it for labels, etc...
    
    type PrimCTM = 
        { CtmTransX : double
          CtmTransY : double
          CtmScaleX : double
          CtmScaleY : double
          CtmRotation : Radians
        }
    
    ///////////// TODO add a radians() constructor
    let identityCTM : PrimCTM = 
        { CtmTransX = 0.0; CtmTransY = 0.0; CtmScaleX = 1.0; CtmScaleY = 1.0; CtmRotation = radians(0.0) }