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

<<<<<<< HEAD
cambiarDinero :: Int -> Accion
cambiarDinero cantidadDinero participante = participante {dinero = dinero participante + cantidadDinero }

cambiarTactica :: String -> Accion
cambiarTactica nuevaTactica participante = participante {tactica = nuevaTactica}

pasarPorElBanco :: Accion
pasarPorElBanco participante = ((cambiarDinero 40). (cambiarTactica "Comprador Compulsivo")) participante

enojarse :: Accion
enojarse participante = cambiarDinero 50 participante { acciones = acciones participante ++ [gritar] }
=======
pasarPorElBanco :: Accion
pasarPorElBanco participante = participante {dinero = dinero participante + 40, tactica = "Comprador Compulsivo"}

enojarse :: Accion
enojarse participante = participante {dinero = dinero participante +50, acciones = acciones participante ++ [gritar] }
>>>>>>> 78e56f5305c27d23daf74f3d1d31d15a7e7e2533

gritar :: Accion
gritar participante = participante {nombre = "AHHHH " ++ nombre participante}

-- como se haria por pattern mattching?
subastar :: Propiedad -> Accion
subastar propiedadAAdquirir participante 
    | esAccionistaUOferente participante = ganarPropiedad propiedadAAdquirir participante
    | otherwise = participante

ganarPropiedad :: Propiedad -> Accion
<<<<<<< HEAD
ganarPropiedad propiedadGanada participante = cambiarDinero (snd propiedadGanada) participante {propiedades = propiedades participante ++ [propiedadGanada] }
=======
ganarPropiedad propiedadGanada participante = participante { dinero = dinero participante - (snd propiedadGanada), propiedades = propiedades participante ++ [propiedadGanada] }
>>>>>>> 78e56f5305c27d23daf74f3d1d31d15a7e7e2533

esAccionistaUOferente :: Participante -> Bool
esAccionistaUOferente participante = tactica participante == "Oferente singular" || tactica participante == "Accionista" 

-- como se haria por pattern mattching?
pagarAAccionistas :: Accion 
pagarAAccionistas participante 
<<<<<<< HEAD
    | tactica participante == "Accionista" = cambiarDinero 200 participante
    | otherwise = cambiarDinero (-100) participante

--esta issue todavia no la modifique. 
=======
    | tactica participante == "Accionista" = participante { dinero = dinero participante + 200 }
    | otherwise = participante { dinero = dinero participante - 100 }


>>>>>>> 78e56f5305c27d23daf74f3d1d31d15a7e7e2533
cobrarAlquileres :: Accion
cobrarAlquileres participante = participante { dinero = dinero participante + (sum.(map esPropiedadBarata).propiedades) participante }

esPropiedadBarata :: Propiedad -> Int
esPropiedadBarata unaPropiedad 
    | snd unaPropiedad < 150 = 10
    | otherwise = 20