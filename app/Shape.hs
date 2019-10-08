module Shape where 
import qualified Vec

data Reflection = Diff | Spec | Refr
                  deriving(Eq, Show)

data Ray = Ray{orig::Vec.Vec, dist::Vec.Vec}
           deriving(Eq, Show)

data Element = 
    Sphere{
        radius      ::  Double,
        position    ::  Vec.Vec,
        emission    ::  Vec.Vec,
        color       ::  Vec.Vec,
        reflection  ::  Reflection}
    | Plain{
        normal :: Vec.Vec} 
    deriving(Eq, Show)


