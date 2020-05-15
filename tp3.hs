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

agregarAccion :: Accion -> Accion
agregarAccion nuevaAccion participante = participante {acciones = nuevaAccion : acciones participante }

cambiarNombre :: String -> Accion
cambiarNombre nuevoNombre participante = participante {nombre = nuevoNombre ++ nombre participante }

agregarPropiedad :: Propiedad -> Accion
agregarPropiedad nuevaPropiedad participante = participante { propiedades = nuevaPropiedad : propiedades participante }

precioPropiedad :: Propiedad -> Int
precioPropiedad (_,precio) = precio

pasarPorElBanco :: Accion
pasarPorElBanco participante = ((cambiarDinero 40). (cambiarTactica "Comprador Compulsivo")) participante

enojarse :: Accion
enojarse participante = ((cambiarDinero 50).(agregarAccion gritar)) participante

gritar :: Accion
gritar participante = cambiarNombre "AHHHH " participante

subastar :: Propiedad -> Accion
subastar propiedadAAdquirir participante 
    | esAccionistaUOferente participante = ganarPropiedad propiedadAAdquirir participante
    | otherwise = participante

ganarPropiedad :: Propiedad -> Accion
ganarPropiedad propiedadGanada participante = (cambiarDinero (-precioPropiedad propiedadGanada).(agregarPropiedad propiedadGanada)) participante

esDeTactica :: String -> Participante -> Bool
esDeTactica unaTactica participante = tactica participante == unaTactica 

esAccionistaUOferente :: Participante -> Bool
esAccionistaUOferente participante = esDeTactica "Oferente singular" participante || esDeTactica "Accionista" participante 

pagarAAccionistas :: Accion 
pagarAAccionistas participante 
    | tactica participante == "Accionista" = cambiarDinero 200 participante
    | otherwise = cambiarDinero (-100) participante

cobrarAlquileres :: Accion
cobrarAlquileres participante = cambiarDinero ((sum.(map precioPorPropiedad).propiedades) participante) participante

precioPorPropiedad :: Propiedad -> Int
precioPorPropiedad unaPropiedad 
    | precioPropiedad unaPropiedad < 150 = 10
    | otherwise = 20

puedeComprarPropiedad :: Int -> Participante -> Bool
puedeComprarPropiedad precioDeUnaPropiedad participante = precioDeUnaPropiedad <= dinero participante

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad participante 
    | puedeComprarPropiedad (precioPropiedad propiedad) participante = ganarPropiedad propiedad participante
    | otherwise = ((hacerBerrinchePor propiedad).gritar.(cambiarDinero 10)) participante

--componer las funciones default del participante
ultimaRonda :: Participante -> Accion
ultimaRonda participante = foldl1 (.) (acciones participante)

--creo que queda mejor el tipado asi que devolver Accion.
--en caso de empate devuelve el segundo participante que le pase. 
juegoFinal :: Participante -> Participante -> Participante
juegoFinal unParticipante otroParticipante
    | dineroUltimaRonda unParticipante > dineroUltimaRonda otroParticipante = jugarUltimaRonda unParticipante
    | otherwise = jugarUltimaRonda otroParticipante

jugarUltimaRonda :: Accion
jugarUltimaRonda participante = (ultimaRonda participante) participante

dineroUltimaRonda :: Participante -> Int
dineroUltimaRonda participante = (dinero.jugarUltimaRonda) participante

