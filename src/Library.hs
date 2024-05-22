module Library where
import PdePreludat
data Cancion = UnaCancion {
    titulo :: String,
    duracion :: Number,
    instrumentos :: [Instrumento]
} deriving (Show, Eq)

data Instrumento = Guitarra | Bajo | Bateria | Teclado | Saxofon deriving (Show, Eq)

patternMatching :: Cancion
patternMatching = UnaCancion "Pattern Matching" 4 [Guitarra, Bajo, Bateria]

seisDieciocho :: Cancion
seisDieciocho = UnaCancion "Seis dieciocho" 3 [Teclado, Guitarra]

laVidaEnHaskell :: Cancion
laVidaEnHaskell = UnaCancion "La vida en Haskell" 5 []

aceptacion :: Cancion -> Number
aceptacion cancion 
    |head (titulo cancion) == 'M' = 500
    |even (length (titulo cancion)) = length (titulo cancion) *10
    |esAcapella cancion = 10
    |otherwise = 0

type Repertorio = [Cancion]
repertorio :: Repertorio
repertorio = [patternMatching, seisDieciocho, seisDieciocho, laVidaEnHaskell, elSensei, amorFuncional]

--Definir dos canciones

elSensei :: Cancion
elSensei = UnaCancion "El Sensei" 3 [Guitarra]

amorFuncional :: Cancion
amorFuncional = UnaCancion "Amor Funcional" 5 [Bajo, Saxofon]

--Que cancion viene antes

vaAntes :: Cancion -> Cancion -> Cancion
vaAntes cancion1 cancion2
    |titulo cancion1 < titulo cancion2 = cancion1
    |otherwise = cancion2

--Es acapella

esAcapella :: Cancion -> Bool
esAcapella cancion = null(instrumentos cancion)

--Es aceptada por el publico

esAceptada :: Cancion -> Bool
esAceptada cancion = aceptacion cancion > 60

--Determinar si la cancion necesita el instrumento

necesitaElInstrumento :: Instrumento -> Cancion -> Bool
necesitaElInstrumento instrumento cancion = instrumento `elem` instrumentos cancion

--Si es aceptada por el publico, se toca completa, sino se reduce a la mitad el tiempo

tocarUnaCancion :: Cancion -> Number
tocarUnaCancion cancion 
    |esAceptada cancion = duracion cancion
    |otherwise = duracion cancion / 2





