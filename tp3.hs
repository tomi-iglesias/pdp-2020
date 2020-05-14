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

agregarPropiedad :: Propiedad -> Accion
agregarPropiedad nuevaPropiedad participante = participante { propiedades = nuevaPropiedad : propiedades participante }

precioPropiedad :: Propiedad -> Int
precioPropiedad (_,precioDePropiedad) = precioDePropiedad

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
ganarPropiedad propiedadGanada participante = (cambiarDinero (precioPropiedad propiedadGanada).(agregarPropiedad propiedadGanada)) participante

esDeTactica :: String -> Participante -> Bool
esDeTactica unaTactica participante = tactica participante == unaTactica 

esAccionistaUOferente :: Participante -> Bool
esAccionistaUOferente participante = esDeTactica "Oferente singular" participante || esDeTactica "Accionista" participante 

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
    | precioPropiedad unaPropiedad < 150 = 10
    | otherwise = 20