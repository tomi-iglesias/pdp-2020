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
cambiarDinero nuevoDinero participante = participante {dinero = dinero participante + nuevoDinero }

cambiarTactica :: String -> Accion
cambiarTactica nuevaTactica participante = participante {tactica = nuevaTactica}

cambiarAcciones :: Accion -> Accion
cambiarAcciones nuevaAccion participante = participante {acciones = nuevaAccion : acciones participante }

cambiarNombre :: String -> Accion
cambiarNombre nuevoNombre participante = participante {nombre = nuevoNombre ++ nombre participante }

pasarPorElBanco :: Accion
pasarPorElBanco participante = ((cambiarDinero 40). (cambiarTactica "Comprador Compulsivo")) participante

enojarse :: Accion
enojarse participante = ((cambiarDinero 50).(cambiarAcciones gritar)) participante

gritar :: Accion
gritar participante = cambiarNombre "AHHHH " participante

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