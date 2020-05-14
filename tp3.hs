import Text.Show.Functions()

main :: IO ()
main = return ()

type Propiedad = (String, Int)
type Accion = Participante -> Participante


data Participante = CrearParticipante { nombre :: String
                                        , dinero :: Int
                                        , tactica :: String
                                        , propiedades :: [Propiedad]
                                        , acciones :: [Accion]
                                    } deriving (Show)



carolina :: Participante
carolina = CrearParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Participante
manuel = CrearParticipante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse] 

cambiarDinero :: Int -> Accion
cambiarDinero cantidadDinero participante = participante {dinero = dinero participante + cantidadDinero }

cambiarTactica :: String -> Accion
cambiarTactica nuevaTactica participante = participante {tactica = nuevaTactica}

pasarPorElBanco :: Accion
pasarPorElBanco participante = ((cambiarDinero 40). (cambiarTactica "Comprador Compulsivo")) participante

enojarse :: Accion
enojarse participante = cambiarDinero 50 participante { acciones = acciones participante ++ [gritar] }

gritar :: Accion
gritar participante = participante {nombre = "AHHHH " ++ nombre participante}

-- como se haria por pattern mattching?
subastar :: Propiedad -> Accion
subastar propiedadAAdquirir participante 
    | esAccionistaUOferente participante = ganarPropiedad propiedadAAdquirir participante
    | otherwise = participante

ganarPropiedad :: Propiedad -> Accion
ganarPropiedad propiedadGanada participante = cambiarDinero (snd propiedadGanada) participante {propiedades = propiedades participante ++ [propiedadGanada] }

esAccionistaUOferente :: Participante -> Bool
esAccionistaUOferente participante = tactica participante == "Oferente singular" || tactica participante == "Accionista" 

-- como se haria por pattern mattching?
pagarAAccionistas :: Accion 
pagarAAccionistas participante 
    | tactica participante == "Accionista" = cambiarDinero 200 participante
    | otherwise = cambiarDinero (-100) participante

--esta issue todavia no la modifique. 
cobrarAlquileres :: Accion
cobrarAlquileres participante = participante { dinero = dinero participante + (sum.(map esPropiedadBarata).propiedades) participante }

esPropiedadBarata :: Propiedad -> Int
esPropiedadBarata unaPropiedad 
    | snd unaPropiedad < 150 = 10
    | otherwise = 20