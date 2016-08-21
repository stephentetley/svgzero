namespace SvgZero

open System

module Geometry = 
    
    
    type Vector2 = 
        | V2 of double * double
        member v.GetX = match v with | V2(x,_) -> x
        
        member v.GetY = match v with | V2(_,y) -> y
        
        static member (+) (v1 : Vector2, v2 : Vector2) = match v1,v2 with | V2(x1,y1), V2(x2,y2) -> V2(x1+x2, y1+y2)
        
        /// TODO - is this functional or does it do in-place mutation?
        member v.Map (f : double -> double) = match v with | V2(x,y) -> V2 (f x, f y)
        
    /// TODO - should we have our own overloaded (mono) fmap for Vector, Point, Matrix... ?
    let vmap (f : double -> double) v1 = match v1 with | V2(x,y) -> V2 (f x, f y)
    
    
    
    
    type Point2 = 
        | P2 of double * double
        
        member p.GetX = match p with | P2(x,_) -> x
        
        member p.GetY = match p with | P2(_,y) -> y
            
            
    type Matrix3x3 = 
        | M3x3 of double * double * double  *  double * double * double  *  double * double * double
        
        static member (+) (m1 : Matrix3x3, m2 : Matrix3x3) = 
            match m1,m2 with
            | M3x3(a,b,c,d,e,f,g,h,i), M3x3(m,n,o,p,q,r,s,t,u) -> 
                M3x3(a+m, b+n, c+o,    d+p, e+q, f+r,    g+s, h+t, i+u)
                      
        
        static member (*) (m1 : Matrix3x3, m2 : Matrix3x3) = 
            match m1,m2 with
            | M3x3(a,b,c,d,e,f,g,h,i), M3x3(m,n,o,p,q,r,s,t,u) ->
                M3x3 (a*m+b*p+c*s,   a*n+b*q+c*t,   a*o+b*r+c*u,
                      d*m+e*p+f*s,   d*n+e*q+f*t,   d*o+e*r+f*u,
                      g*m+h*p+i*s,   g*n+h*q+i*t,   g*o+h*r+i*u)

    let lift2Vector2 (op : double -> double -> double) (v1 : Vector2) (v2 : Vector2) = 
        match v1,v2 with
            | V2 (x1,y1), V2 (x2,y2) -> V2 (op x1 x2, op y1 y2)

         
            
    /// TODO - look at INumeric...
    
    let vectorAdd v1 v2 = lift2Vector2 (+) v1 v2
    let vectorSub v1 v2 = lift2Vector2 (-) v1 v2
    
    let (vreverse : Vector2 -> Vector2) = function | V2(x, y) -> V2 (-x, -y)
            
            
    let lift2Matrix3x3 (op : double -> double -> double) m1 m2 = 
        match m1,m2 with
        | M3x3(a,b,c,d,e,f,g,h,i), M3x3 (m,n,o,p,q,r,s,t,u) -> 
            M3x3(op a m, op b n, op c o,   op d p, op e q, op f r,   op g s, op h t, op i u)
                                                                         
    
    /// Matching on this form is often more useful than Matrix3x3 (use Active pattern?)
    let deconsMatrix (m : Matrix3x3) = 
        match m with
            | M3x3(e0x,e1x,ox,  e0y,e1y,oy,  _,_,_) -> (e0x,e0y, e1x,e1y, ox,oy)
        
    let transpose (m : Matrix3x3) = 
        match m with 
            | M3x3(a,b,c,  d,e,f,  g,h,i) -> M3x3(a,d,g,  b,e,h,  c,f,i)
            
    let (mofm : Matrix3x3 -> Matrix3x3) = function
        | M3x3(a,b,c,  d,e,f,  g,h,i) -> 
            let m11 = (e*i) - (f*h)
            let m12 = (d*i) - (f*g)
            let m13 = (d*h) - (e*g)
            let m21 = (b*i) - (c*h)
            let m22 = (a*i) - (c*g)
            let m23 = (a*h) - (b*g)
            let m31 = (b*f) - (c*e)
            let m32 = (a*f) - (c*d)
            let m33 = (a*e) - (b*d)
            M3x3(m11,m12,m13,  m21,m22,m23,  m31,m32,m33)

    let cofactor : Matrix3x3 -> Matrix3x3 = function
        | M3x3(a,b,c,  d,e,f,  g,h,i) -> M3x3(a,(-b),c,  (-d),e,(-f),   g,(-h),i)
        
    let (adjoint : Matrix3x3 -> Matrix3x3) = transpose << cofactor << mofm
    
    let determinant : Matrix3x3 -> double = function
        | M3x3(a,b,c,  d,e,f,  g,h,i) -> a*e*i - a*f*h - b*d*i + b*f*g + c*d*h - c*e*g
    
    /// Naming - Radian or Radians?
    type Radians = Rad of double
    
    /// At some point the Rad constructor should be opaque...
    let radians (d : double) = Rad(d)

    let deg2Rad (d : double) = Rad(d * Math.PI / 180.0)
    
    let rad2Deg : Radians -> double = function | Rad(r) -> r * (180.0 / Math.PI)
            
            
    /// lineDirection :: (Floating u, Real u) => Point2 u -> Point2 u -> Radian
    /// Special cases for continuity - the equality test should 
    /// catch both 0.0 and (-0.0).
    /// Note - there is undoubtedly a better way of doing this.
    let lineDirection p1 p2 = 
        Rad <| match p1,p2 with 
                | P2(x1,y1), P2(x2,y2) ->
                let pve = fun x -> x >= 0.0
                match (x2 - x1, y2 - y1) with
                | (x,y) when x <> 0.0 && y <> 0.0 -> 0.0
                | (x,y) when x <> 0.0 -> if y >=0.0 then 0.5 * Math.PI else 1.5 * Math.PI
                | (x,y) when y <> 0.0 -> if x >= 0.0 then 0.0 else Math.PI

                /// north-east quadrant 
                | (x,y) when pve x && pve y -> atan (y/x)          
    
                /// north-west quadrant
                | (x,y) when pve y -> Math.PI - (atan (y / abs x))

                /// south-east quadrant
                | (x,y) when pve x -> (2.0 * Math.PI) - (atan (abs y / x)) 

                /// otherwise... south-west quadrant
                | (x,y) -> Math.PI + (atan (y/x))


    let identityMatrix : Matrix3x3 = M3x3(1.0, 0.0, 0.0,   0.0, 1.0, 0.0,  0.0, 0.0 ,1.0)
    
    let scalingMatrix (sx : double) (sy : double) : Matrix3x3 = 
        M3x3 (sx, 0.0, 0.0,  0.0, sy, 0.0,  0.0, 0.0, 1.0)
    
    let translationMatrix (dx : double) (dy : double) : Matrix3x3 = 
        M3x3(1.0, 0.0, dx,  0.0, 1.0, dy,  0.0, 0.0, 1.0)
    
    let rotationMatrix : Radians -> Matrix3x3 = 
        function | Rad(ang) -> M3x3 (cos ang, -(sin ang), 0.0, 
                                     sin ang, cos ang, 0.0,  
                                     0.0, 0.0, 1.0)
                  
    let originatedRotationMatrix (ang : Radians) (pt : Point2) = 
        match pt with
        | P2(x,y) -> 
            let mT = translationMatrix x y
            let mTinv = translationMatrix (-x) (-y)
            mT * rotationMatrix ang * mTinv        

