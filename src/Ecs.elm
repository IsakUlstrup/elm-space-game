module Ecs exposing
    ( Entity
    , Scene
    , System
    , addEntity
    , addSystem
    , emptyScene
    , entities
    , entityId
    , filterEntities
    , foldScene
    , getComponents
    , mapComponents
    , mapEntities
    , removeComponent
    , removeEntity
    , runSystems
    , seed
    , setSeed
    , updateComponent
    , updateEntity
    )

import Random


type Entity c
    = Entity
        { id : Int
        , components : List c
        }


type System c msg
    = System (msg -> Scene c msg -> Scene c msg)


type Scene c msg
    = Scene
        { entities : List (Entity c)
        , systems : List (System c msg)
        , seed : Random.Seed
        , idCounter : Int
        }


emptyScene : Scene c msg
emptyScene =
    Scene
        { entities = []
        , systems = []
        , seed = Random.initialSeed 20
        , idCounter = 0
        }


foldScene : (Entity c -> Scene c msg -> Scene c msg) -> Scene c msg -> List (Entity c) -> Scene c msg
foldScene func scene ents =
    case ents of
        [] ->
            scene

        e :: es ->
            func e (foldScene func scene es)


addEntity : List c -> Scene c msg -> Scene c msg
addEntity components (Scene scene) =
    Scene
        { scene
            | entities = Entity { id = scene.idCounter, components = components } :: scene.entities
            , idCounter = scene.idCounter + 1
        }



-- maybe manage this, so we can't mess up seeds


seed : Scene c msg -> Random.Seed
seed (Scene scene) =
    scene.seed


setSeed : Random.Seed -> Scene c msg -> Scene c msg
setSeed sd (Scene scene) =
    Scene { scene | seed = sd }


entities : Scene c msg -> List (Entity c)
entities (Scene scene) =
    scene.entities


removeEntity : Entity c -> Scene c msg -> Scene c msg
removeEntity entity (Scene scene) =
    Scene { scene | entities = List.filter (\e -> e /= entity) scene.entities }



{- Filter out any entities that do not meet perdicate -}


filterEntities : (Entity c -> Bool) -> Scene c msg -> Scene c msg
filterEntities pred (Scene scene) =
    Scene { scene | entities = List.filter pred scene.entities }


mapEntities : (Entity c -> Entity c) -> Scene c msg -> Scene c msg
mapEntities func (Scene scene) =
    Scene { scene | entities = List.map func scene.entities }


updateEntity : Entity c -> Scene c msg -> Scene c msg
updateEntity (Entity entity) (Scene scene) =
    let
        update (Entity e) =
            if e.id == entity.id then
                Entity { e | components = entity.components }

            else
                Entity e
    in
    Scene { scene | entities = List.map update scene.entities }


entityId : Entity c -> Int
entityId (Entity entity) =
    entity.id


getComponents : Entity c -> List c
getComponents (Entity entity) =
    entity.components


mapComponents : (c -> c) -> Entity c -> Entity c
mapComponents func (Entity entity) =
    Entity { entity | components = List.map func entity.components }


removeComponent : c -> (c -> c -> Bool) -> Entity c -> Entity c
removeComponent cd hasComponent (Entity entity) =
    Entity { entity | components = List.filter (\c -> hasComponent cd c |> not) entity.components }


updateComponent : c -> (c -> c -> Bool) -> Entity c -> Entity c
updateComponent cd hasComponent (Entity entity) =
    Entity { entity | components = cd :: List.filter (\c -> hasComponent cd c |> not) entity.components }


addSystem : (msg -> Scene c msg -> Scene c msg) -> Scene c msg -> Scene c msg
addSystem system (Scene scene) =
    Scene { scene | systems = System system :: scene.systems }


runSystem : msg -> System c msg -> Scene c msg -> Scene c msg
runSystem msg (System system) scene =
    system msg scene


runSystems : msg -> Scene c msg -> Scene c msg
runSystems msg ((Scene scene) as sc) =
    List.foldl (runSystem msg) sc scene.systems
