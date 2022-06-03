module Id where
import Type

type Id = String

newtype Label = Label String deriving (Eq, Show)

genId :: String -> Int -> (Id, Int)
genId s counter = let counter' = counter + 1
                  in (s ++ "." ++ show counter', counter')

idOfType :: TypeF f -> String
idOfType Unit = "u"
idOfType Bool = "b"
idOfType Int = "i"
idOfType Float = "d"
idOfType (Fun _ _) = "f"
idOfType (Tuple _) = "t"
idOfType (Array _) = "a"
idOfType (Var _) = error "idOfType var"

genTmp :: TypeF f -> Int -> (Id, Int)
genTmp typ counter = let counter' = counter + 1
                     in ("T" ++ idOfType typ ++ show counter', counter')
