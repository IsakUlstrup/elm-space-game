module Ecs exposing
    ( Entity
    , Scene
    , System
    , addEntity
    , addSystem
    , emptyScene
    , getComponentsOfType
    , getEntitiesWithComponents
    , runSystems
    )

import Random


{-| Id alias for Entity/Component ids
-}
type alias Id =
    Int


{-| Component, compData is the type stored in each component, usually a union type

This type is not exposed to avoid mixing up Component and compData

-}
type Component compData
    = Component
        { id : Id
        , parent : Entity
        , data : compData
        }


{-| Entity is just an id used to associate entities and components
-}
type Entity
    = Entity Id


{-| A system is a function that transforms the scene when given a message
-}
type System compData msg
    = System (msg -> Scene compData msg -> Scene compData msg)


{-| A Scene is a container for entities, components and systems
, as well as an id counter to keep track of new ids and a Random seed to help with RNG
-}
type Scene compData msg
    = Scene
        { entities : List Entity
        , components : List (Component compData)
        , systems : List (System compData msg)
        , seed : Random.Seed
        , idCounter : Id
        }



---- SCENE ----


{-| Construct new empty scene
-}
emptyScene : Int -> Scene compData msg
emptyScene seedInit =
    Scene
        { entities = []
        , components = []
        , systems = []
        , seed = Random.initialSeed seedInit
        , idCounter = 0
        }



---- ENTITY ----


addEntity : List compData -> Scene compData msg -> Scene compData msg
addEntity components (Scene scene) =
    let
        entity =
            Entity scene.idCounter
    in
    Scene
        { scene
            | entities = entity :: scene.entities
            , idCounter = scene.idCounter + 1
        }
        |> addComponents entity components


{-| Get a list of entities with their components, useful for rendering
-}
getEntitiesWithComponents : Scene compData msg -> List ( Entity, List compData )
getEntitiesWithComponents (Scene scene) =
    let
        compData : Entity -> Component compData -> Maybe compData
        compData e (Component c) =
            if c.parent == e then
                Just c.data

            else
                Nothing
    in
    List.map
        (\e ->
            ( e, List.filterMap (compData e) scene.components )
        )
        scene.entities



---- COMPONENT ----


{-| Create a new component with parent as parent and compData as data
-}
addComponent : Entity -> compData -> Scene compData msg -> Scene compData msg
addComponent parent compData (Scene scene) =
    Scene
        { scene
            | components = Component { id = scene.idCounter, parent = parent, data = compData } :: scene.components
            , idCounter = scene.idCounter + 1
        }


{-| Add list of component data with parent as parent
-}
addComponents : Entity -> List compData -> Scene compData msg -> Scene compData msg
addComponents parent compDatas scene =
    case compDatas of
        [] ->
            scene

        d :: ds ->
            addComponent parent d (addComponents parent ds scene)


{-| Given a filter function and a list of Components, return compData that passes filter
-}
getComponentsOfType : (compData -> Bool) -> List (Component compData) -> List compData
getComponentsOfType filter comps =
    let
        getCompData : Component compData -> Maybe compData
        getCompData (Component c) =
            if filter c.data then
                Just c.data

            else
                Nothing
    in
    List.filterMap
        getCompData
        comps



---- SYSTEM ----


{-| Add a system to a scene
-}
addSystem : (msg -> Scene c msg -> Scene c msg) -> Scene c msg -> Scene c msg
addSystem system (Scene scene) =
    Scene { scene | systems = System system :: scene.systems }


{-| Run all systems in a scene and return the resulting scene
-}
runSystems : msg -> Scene c msg -> Scene c msg
runSystems msg ((Scene scene) as sc) =
    let
        runSystem : msg -> System c msg -> Scene c msg -> Scene c msg
        runSystem m (System system) s =
            system m s
    in
    List.foldl (runSystem msg) sc scene.systems
