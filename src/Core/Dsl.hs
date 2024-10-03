module Core.Dsl (module Core.Dsl) where

import Core.Model

scene :: [Node] -> Scene
scene nodes =
  Scene
    { name = Nothing,
      nodes
    }

primitive :: Primitive -> Node
primitive p =
  defaultNode
    { mesh =
        pure $
          Mesh
            { name = Nothing,
              primitives = [p]
            }
    }
