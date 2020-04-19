-- Defino tipos de datos, creo tres ejemplos de planetas y tres ejemplos de enfermedades

type TasaMortalidad = Float
type MedidaCombate = String
type Continente = String
type CantidadPoblacion = Int
type Victimas = Int
type Mortalidad = Float

data Enfermedad = Enfermedad {
    tasaDeMortalidad::TasaMortalidad,
    medidaDeCombate::MedidaCombate
} deriving Show;

data Planeta = Planeta {
    poblacionTotal::CantidadPoblacion,
    continentes::[Continente],
    medidasTomadas::[MedidaCombate]
} deriving Show;

-- Ejemplo de Enfermedades

coronaVirus :: Enfermedad
coronaVirus = Enfermedad 3.5 "cuarentena"

dengue :: Enfermedad
dengue = Enfermedad 2.7 "Evitar la acumulacion de agua"

peste :: Enfermedad
peste = Enfermedad 0.01 "lavarse las manos"

-- Ejemplos de Planetas

tierraAntesDeCuarentena :: Planeta
tierraAntesDeCuarentena = Planeta 7777382699 ["America", "Asia", "Africa", "Europa", "Oceania", "Antartida"] ["lavarse las manos"]

marte :: Planeta
marte = Planeta 2 [] ["cuarentena","lavarse las manos","Evitar la acumulacion de agua"]

venus :: Planeta
venus = Planeta 15 ["Comuna 1","Comuna 2","Comuna 3","Comuna 4","Comuna 5"] ["cuarentena","evitar lugares cerrados","usar barbijo","lavarse las manos"]

-- Punto 1: 

combinarEnfermedades:: Enfermedad -> Enfermedad -> Enfermedad
combinarEnfermedades enfermedad1 enfermedad2 = Enfermedad (obtenerMortalidadConjunta enfermedad1 enfermedad2) (medidaDeCombate enfermedad1)

obtenerMortalidadConjunta::Enfermedad -> Enfermedad -> TasaMortalidad
obtenerMortalidadConjunta enfermedad1 enfermedad2 = tasaDeMortalidad enfermedad1 + tasaDeMortalidad enfermedad2

-- Punto 2

tomarMedidasDeProteccion :: Planeta -> Enfermedad -> Planeta
tomarMedidasDeProteccion planeta enfermedad | not (planetaProtegidoContra planeta enfermedad) = implementarMedida planeta (medidaDeCombate enfermedad)
                                            | otherwise = planeta

planetaProtegidoContra :: Planeta -> Enfermedad -> Bool                                            
planetaProtegidoContra planeta enfermedad = planetaImplementaMedida planeta (medidaDeCombate enfermedad)

planetaImplementaMedida:: Planeta -> MedidaCombate -> Bool
planetaImplementaMedida planeta medida = elem medida (medidasTomadas planeta)

implementarMedida :: Planeta -> MedidaCombate -> Planeta
implementarMedida planeta medida = planeta{medidasTomadas = (medidasTomadas planeta) ++ [medida] }

-- Punto 3

estaAlHorno :: Planeta -> Enfermedad -> Bool
estaAlHorno planeta enfermedad = not (planetaProtegidoContra planeta enfermedad) && (cantidadDeVictimas planeta enfermedad) > 1000000

cantidadDeVictimas :: Planeta -> Enfermedad -> Victimas
cantidadDeVictimas planeta enfermedad = ceiling ( mortalidad (tasaDeMortalidad enfermedad) (poblacionTotal planeta) ) 

mortalidad :: TasaMortalidad -> CantidadPoblacion -> Mortalidad
mortalidad tasa cantidadPersonas = ( tasa * fromIntegral (cantidadPersonas) ) / 100

-- Punto 4
tieneMasMedidasQueHabitantesPorContinente :: Planeta -> Bool
tieneMasMedidasQueHabitantesPorContinente planeta = cantidadMedidasPrevencion planeta > cantidadHabitantesPorContinente planeta

cantidadMedidasPrevencion :: Planeta -> Int
cantidadMedidasPrevencion planeta = length (medidasTomadas planeta)

cantidadHabitantesPorContinente :: Planeta -> Int
cantidadHabitantesPorContinente planeta | tieneContinentes planeta = div (poblacionTotal planeta) (cantidadDeContinentes planeta)
                                        | otherwise = poblacionTotal planeta

tieneContinentes :: Planeta -> Bool
tieneContinentes planeta = cantidadDeContinentes planeta > 0

cantidadDeContinentes :: Planeta -> Int
cantidadDeContinentes planeta = length (continentes planeta)

-- Punto 5

type PlanetaAtacado = (Planeta,Victimas)

enfermedadAtacaPlaneta :: Enfermedad -> Planeta -> PlanetaAtacado
enfermedadAtacaPlaneta enfermedad planeta | planetaProtegidoContra planeta enfermedad = (planeta,0)
                                          | otherwise = ( planetaDespuesDelAtaque planeta enfermedad , cantidadDeVictimas planeta enfermedad )

planetaDespuesDelAtaque :: Planeta -> Enfermedad -> Planeta
planetaDespuesDelAtaque planeta enfermedad = tomarMedidasDeProteccion ( atacarPlaneta planeta enfermedad ) enfermedad

atacarPlaneta :: Planeta -> Enfermedad -> Planeta
atacarPlaneta planeta enfermedad = planeta{ poblacionTotal = (poblacionTotal planeta) - (cantidadDeVictimas planeta enfermedad) }

-- Punto 6
 
tieneMasVictimasLaPrimeraCombinadaConLaSegunda :: Enfermedad -> Enfermedad -> Planeta -> Bool
tieneMasVictimasLaPrimeraCombinadaConLaSegunda enfermedad1 enfermedad2 planeta = 
    victimasAtaqueCombinado enfermedad1 enfermedad2 planeta > victimasAtaqueCombinado enfermedad2 enfermedad1 planeta

victimasAtaqueCombinado :: Enfermedad -> Enfermedad -> Planeta -> Victimas
victimasAtaqueCombinado enfermedad1 enfermedad2 planeta = snd ( enfermedadAtacaPlaneta (combinarEnfermedades enfermedad1 enfermedad2) planeta )
