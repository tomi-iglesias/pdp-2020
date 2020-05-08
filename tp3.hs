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

pasarPorElBanco :: Accion
pasarPorElBanco participante = participante {dinero = dinero participante + 40, tactica = "Comprador Compulsivo"}

enojarse :: Accion
enojarse participante = participante {dinero = dinero participante +50, acciones = acciones participante ++ [gritar] }

gritar :: Accion
gritar participante = participante {nombre = "AHHHH " ++ nombre participante}

-- como se haria por pattern mattching??
subastar :: Propiedad -> Accion
subastar propiedadAAdquirir participante 
    | esAccionistaUOferente participante = ganarPropiedad propiedadAAdquirir participante
    | otherwise = participante

ganarPropiedad :: Propiedad -> Accion
ganarPropiedad propiedadGanada participante = participante { dinero = dinero participante - (snd propiedadGanada), propiedades = propiedades participante ++ [propiedadGanada] }

esAccionistaUOferente :: Participante -> Bool
esAccionistaUOferente participante = tactica participante == "Oferente singular" || tactica participante == "Accionista" 

-- como se haria por pattern mattching?
pagarAAccionistas :: Accion 
pagarAAccionistas participante 
    | tactica participante == "Accionista" = participante { dinero = dinero participante + 200 }
    | otherwise = participante { dinero = dinero participante - 100 }


cobrarAlquileres :: Accion
cobrarAlquileres participante = participante { dinero = dinero participante + (sum.(map esPropiedadBarata).propiedades) participante }

esPropiedadBarata :: Propiedad -> Int
esPropiedadBarata unaPropiedad 
    | snd unaPropiedad < 150 = 10
    | otherwise = 20